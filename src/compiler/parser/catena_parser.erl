%% This file is generated from catena_parser.yrl. Do not edit directly.
-file("src/compiler/parser/catena_parser.yrl", 0).
-module(catena_parser).
-file("src/compiler/parser/catena_parser.erl", 4).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/compiler/parser/catena_parser.yrl", 1051).

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
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'module', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function,  yeccpars2_1/7}).
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function,  yeccpars2_2/7}).
yeccpars2_2(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_5(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function,  yeccpars2_6/7}).
yeccpars2_6(S, 'export', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_module_header(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function,  yeccpars2_7/7}).
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function,  yeccpars2_8/7}).
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function,  yeccpars2_9/7}).
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_catena_module(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function,  yeccpars2_10/7}).
yeccpars2_10(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_declarations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function,  yeccpars2_11/7}).
yeccpars2_11(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function,  yeccpars2_12/7}).
yeccpars2_12(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 357, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 358, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_14(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function,  yeccpars2_14/7}).
yeccpars2_cont_14(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_14(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function,  yeccpars2_15/7}).
yeccpars2_15(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 324, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function,  yeccpars2_16/7}).
yeccpars2_16(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function,  yeccpars2_17/7}).
yeccpars2_17(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_18(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function,  yeccpars2_20/7}).
yeccpars2_20(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccpars2_22(22, Cat, [20 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_21/7}).
-compile({nowarn_unused_function,  yeccpars2_21/7}).
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_type_params(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_type_params_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function,  yeccpars2_24/7}).
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_type_params_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function,  yeccpars2_26/7}).
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function,  yeccpars2_27/7}).
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function,  yeccpars2_28/7}).
yeccpars2_28(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_constructors(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function,  yeccpars2_29/7}).
yeccpars2_29(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_constructor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_constructor_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function,  yeccpars2_31/7}).
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_constructor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function,  yeccpars2_32/7}).
yeccpars2_32(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function,  yeccpars2_33/7}).
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_14(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function,  yeccpars2_36/7}).
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_type_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_37/7}).
-compile({nowarn_unused_function,  yeccpars2_37/7}).
yeccpars2_37(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_type_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).
-compile({nowarn_unused_function,  yeccpars2_38/7}).
yeccpars2_38(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function,  yeccpars2_39/7}).
yeccpars2_39(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccpars2_46(46, Cat, [39 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function,  yeccpars2_40/7}).
yeccpars2_40(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function,  yeccpars2_41/7}).
yeccpars2_41(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function,  yeccpars2_44/7}).
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_type_expr_primary_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).
-compile({nowarn_unused_function,  yeccpars2_45/7}).
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function,  yeccpars2_46/7}).
yeccpars2_46(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_47: see yeccpars2_34

-dialyzer({nowarn_function, yeccpars2_48/7}).
-compile({nowarn_unused_function,  yeccpars2_48/7}).
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_49: see yeccpars2_34

yeccpars2_50(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_14(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
-compile({nowarn_unused_function,  yeccpars2_51/7}).
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_trait_constraint(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_52/7}).
-compile({nowarn_unused_function,  yeccpars2_52/7}).
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_53/7}).
-compile({nowarn_unused_function,  yeccpars2_53/7}).
yeccpars2_53(S, 'ampersand', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_type_constraints(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_54: see yeccpars2_50

-dialyzer({nowarn_function, yeccpars2_55/7}).
-compile({nowarn_unused_function,  yeccpars2_55/7}).
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_type_constraints(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_56/7}).
-compile({nowarn_unused_function,  yeccpars2_56/7}).
yeccpars2_56(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_57: see yeccpars2_34

-dialyzer({nowarn_function, yeccpars2_58/7}).
-compile({nowarn_unused_function,  yeccpars2_58/7}).
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_59/7}).
-compile({nowarn_unused_function,  yeccpars2_59/7}).
yeccpars2_59(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_60/7}).
-compile({nowarn_unused_function,  yeccpars2_60/7}).
yeccpars2_60(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_type_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_61: see yeccpars2_34

-dialyzer({nowarn_function, yeccpars2_62/7}).
-compile({nowarn_unused_function,  yeccpars2_62/7}).
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_type_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_63/7}).
-compile({nowarn_unused_function,  yeccpars2_63/7}).
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_64/7}).
-compile({nowarn_unused_function,  yeccpars2_64/7}).
yeccpars2_64(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_65/7}).
-compile({nowarn_unused_function,  yeccpars2_65/7}).
yeccpars2_65(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_66/7}).
-compile({nowarn_unused_function,  yeccpars2_66/7}).
yeccpars2_66(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_67/7}).
-compile({nowarn_unused_function,  yeccpars2_67/7}).
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_68/7}).
-compile({nowarn_unused_function,  yeccpars2_68/7}).
yeccpars2_68(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_effect_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_69/7}).
-compile({nowarn_unused_function,  yeccpars2_69/7}).
yeccpars2_69(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_70/7}).
-compile({nowarn_unused_function,  yeccpars2_70/7}).
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_effect_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_71/7}).
-compile({nowarn_unused_function,  yeccpars2_71/7}).
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_72/7}).
-compile({nowarn_unused_function,  yeccpars2_72/7}).
yeccpars2_72(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_73/7}).
-compile({nowarn_unused_function,  yeccpars2_73/7}).
yeccpars2_73(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_type_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_74/7}).
-compile({nowarn_unused_function,  yeccpars2_74/7}).
yeccpars2_74(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_75/7}).
-compile({nowarn_unused_function,  yeccpars2_75/7}).
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_76: see yeccpars2_34

-dialyzer({nowarn_function, yeccpars2_77/7}).
-compile({nowarn_unused_function,  yeccpars2_77/7}).
yeccpars2_77(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_type_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_78/7}).
-compile({nowarn_unused_function,  yeccpars2_78/7}).
yeccpars2_78(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_79/7}).
-compile({nowarn_unused_function,  yeccpars2_79/7}).
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_type_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_80/7}).
-compile({nowarn_unused_function,  yeccpars2_80/7}).
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_81/7}).
-compile({nowarn_unused_function,  yeccpars2_81/7}).
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_constructor_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_82: see yeccpars2_25

-dialyzer({nowarn_function, yeccpars2_83/7}).
-compile({nowarn_unused_function,  yeccpars2_83/7}).
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_constructors(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_84: see yeccpars2_25

-dialyzer({nowarn_function, yeccpars2_85/7}).
-compile({nowarn_unused_function,  yeccpars2_85/7}).
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_type_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_86/7}).
-compile({nowarn_unused_function,  yeccpars2_86/7}).
yeccpars2_86(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 300, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_87/7}).
-compile({nowarn_unused_function,  yeccpars2_87/7}).
yeccpars2_87(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 yeccpars2_89(89, Cat, [87 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_88/7}).
-compile({nowarn_unused_function,  yeccpars2_88/7}).
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_pattern_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_89/7}).
-compile({nowarn_unused_function,  yeccpars2_89/7}).
yeccpars2_89(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_90/7}).
-compile({nowarn_unused_function,  yeccpars2_90/7}).
yeccpars2_90(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_91: see yeccpars2_34

-dialyzer({nowarn_function, yeccpars2_92/7}).
-compile({nowarn_unused_function,  yeccpars2_92/7}).
yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_93/7}).
-compile({nowarn_unused_function,  yeccpars2_93/7}).
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
-compile({nowarn_unused_function,  yeccpars2_94/7}).
yeccpars2_94(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_95(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_96/7}).
-compile({nowarn_unused_function,  yeccpars2_96/7}).
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
-compile({nowarn_unused_function,  yeccpars2_97/7}).
yeccpars2_97(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

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
yeccpars2_100(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_101/7}).
-compile({nowarn_unused_function,  yeccpars2_101/7}).
yeccpars2_101(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_102/7}).
-compile({nowarn_unused_function,  yeccpars2_102/7}).
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_103/7}).
-compile({nowarn_unused_function,  yeccpars2_103/7}).
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_105/7}).
-compile({nowarn_unused_function,  yeccpars2_105/7}).
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_106/7}).
-compile({nowarn_unused_function,  yeccpars2_106/7}).
yeccpars2_106(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccpars2_109(109, Cat, [106 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
-compile({nowarn_unused_function,  yeccpars2_107/7}).
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).
-compile({nowarn_unused_function,  yeccpars2_108/7}).
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_pattern_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).
-compile({nowarn_unused_function,  yeccpars2_109/7}).
yeccpars2_109(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_110(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_111/7}).
-compile({nowarn_unused_function,  yeccpars2_111/7}).
yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_112: see yeccpars2_97

-dialyzer({nowarn_function, yeccpars2_113/7}).
-compile({nowarn_unused_function,  yeccpars2_113/7}).
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_114/7}).
-compile({nowarn_unused_function,  yeccpars2_114/7}).
yeccpars2_114(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_115/7}).
-compile({nowarn_unused_function,  yeccpars2_115/7}).
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_116/7}).
-compile({nowarn_unused_function,  yeccpars2_116/7}).
yeccpars2_116(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_117/7}).
-compile({nowarn_unused_function,  yeccpars2_117/7}).
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_118/7}).
-compile({nowarn_unused_function,  yeccpars2_118/7}).
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_pattern_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_119/7}).
-compile({nowarn_unused_function,  yeccpars2_119/7}).
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_120: see yeccpars2_97

-dialyzer({nowarn_function, yeccpars2_121/7}).
-compile({nowarn_unused_function,  yeccpars2_121/7}).
yeccpars2_121(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_122/7}).
-compile({nowarn_unused_function,  yeccpars2_122/7}).
yeccpars2_122(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_123/7}).
-compile({nowarn_unused_function,  yeccpars2_123/7}).
yeccpars2_123(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_124: see yeccpars2_97

-dialyzer({nowarn_function, yeccpars2_125/7}).
-compile({nowarn_unused_function,  yeccpars2_125/7}).
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_126/7}).
-compile({nowarn_unused_function,  yeccpars2_126/7}).
yeccpars2_126(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_127/7}).
-compile({nowarn_unused_function,  yeccpars2_127/7}).
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_128/7}).
-compile({nowarn_unused_function,  yeccpars2_128/7}).
yeccpars2_128(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_129/7}).
-compile({nowarn_unused_function,  yeccpars2_129/7}).
yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_129_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_130/7}).
-compile({nowarn_unused_function,  yeccpars2_130/7}).
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_131/7}).
-compile({nowarn_unused_function,  yeccpars2_131/7}).
yeccpars2_131(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_132/7}).
-compile({nowarn_unused_function,  yeccpars2_132/7}).
yeccpars2_132(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_record_pattern_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_133/7}).
-compile({nowarn_unused_function,  yeccpars2_133/7}).
yeccpars2_133(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_134/7}).
-compile({nowarn_unused_function,  yeccpars2_134/7}).
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_135: see yeccpars2_97

-dialyzer({nowarn_function, yeccpars2_136/7}).
-compile({nowarn_unused_function,  yeccpars2_136/7}).
yeccpars2_136(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_record_pattern_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_137/7}).
-compile({nowarn_unused_function,  yeccpars2_137/7}).
yeccpars2_137(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_138/7}).
-compile({nowarn_unused_function,  yeccpars2_138/7}).
yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_record_pattern_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_139/7}).
-compile({nowarn_unused_function,  yeccpars2_139/7}).
yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_140/7}).
-compile({nowarn_unused_function,  yeccpars2_140/7}).
yeccpars2_140(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_transform_signature(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_141(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_141(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_141/7}).
-compile({nowarn_unused_function,  yeccpars2_141/7}).
yeccpars2_cont_141(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_142/7}).
-compile({nowarn_unused_function,  yeccpars2_142/7}).
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_143: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_144/7}).
-compile({nowarn_unused_function,  yeccpars2_144/7}).
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_145/7}).
-compile({nowarn_unused_function,  yeccpars2_145/7}).
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_146/7}).
-compile({nowarn_unused_function,  yeccpars2_146/7}).
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_147/7}).
-compile({nowarn_unused_function,  yeccpars2_147/7}).
yeccpars2_147(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_148/7}).
-compile({nowarn_unused_function,  yeccpars2_148/7}).
yeccpars2_148(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_guards(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_149/7}).
-compile({nowarn_unused_function,  yeccpars2_149/7}).
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 yeccgoto_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_150/7}).
-compile({nowarn_unused_function,  yeccpars2_150/7}).
yeccpars2_150(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_151/7}).
-compile({nowarn_unused_function,  yeccpars2_151/7}).
yeccpars2_151(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_guard(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_152/7}).
-compile({nowarn_unused_function,  yeccpars2_152/7}).
yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_153/7}).
-compile({nowarn_unused_function,  yeccpars2_153/7}).
yeccpars2_153(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_154/7}).
-compile({nowarn_unused_function,  yeccpars2_154/7}).
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_155/7}).
-compile({nowarn_unused_function,  yeccpars2_155/7}).
yeccpars2_155(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_156: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_157/7}).
-compile({nowarn_unused_function,  yeccpars2_157/7}).
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_158/7}).
-compile({nowarn_unused_function,  yeccpars2_158/7}).
yeccpars2_158(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_159(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_141(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_160/7}).
-compile({nowarn_unused_function,  yeccpars2_160/7}).
yeccpars2_160(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_161/7}).
-compile({nowarn_unused_function,  yeccpars2_161/7}).
yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_161_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_162: see yeccpars2_141

yeccpars2_163(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_141(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_164/7}).
-compile({nowarn_unused_function,  yeccpars2_164/7}).
yeccpars2_164(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_165/7}).
-compile({nowarn_unused_function,  yeccpars2_165/7}).
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_166/7}).
-compile({nowarn_unused_function,  yeccpars2_166/7}).
yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_166_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_167/7}).
-compile({nowarn_unused_function,  yeccpars2_167/7}).
yeccpars2_167(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_168/7}).
-compile({nowarn_unused_function,  yeccpars2_168/7}).
yeccpars2_168(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_169/7}).
-compile({nowarn_unused_function,  yeccpars2_169/7}).
yeccpars2_169(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_170/7}).
-compile({nowarn_unused_function,  yeccpars2_170/7}).
yeccpars2_170(S, 'do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 yeccpars2_171(171, Cat, [170 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_171/7}).
-compile({nowarn_unused_function,  yeccpars2_171/7}).
yeccpars2_171(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_172/7}).
-compile({nowarn_unused_function,  yeccpars2_172/7}).
yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_expr_list_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_173/7}).
-compile({nowarn_unused_function,  yeccpars2_173/7}).
yeccpars2_173(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_174: see yeccpars2_141

%% yeccpars2_175: see yeccpars2_141

%% yeccpars2_176: see yeccpars2_141

%% yeccpars2_177: see yeccpars2_141

%% yeccpars2_178: see yeccpars2_141

%% yeccpars2_179: see yeccpars2_141

%% yeccpars2_180: see yeccpars2_141

%% yeccpars2_181: see yeccpars2_141

%% yeccpars2_182: see yeccpars2_141

%% yeccpars2_183: see yeccpars2_141

%% yeccpars2_184: see yeccpars2_141

%% yeccpars2_185: see yeccpars2_141

%% yeccpars2_186: see yeccpars2_141

%% yeccpars2_187: see yeccpars2_141

%% yeccpars2_188: see yeccpars2_141

%% yeccpars2_189: see yeccpars2_141

%% yeccpars2_190: see yeccpars2_141

%% yeccpars2_191: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_192/7}).
-compile({nowarn_unused_function,  yeccpars2_192/7}).
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_193/7}).
-compile({nowarn_unused_function,  yeccpars2_193/7}).
yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_194/7}).
-compile({nowarn_unused_function,  yeccpars2_194/7}).
yeccpars2_194(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_194_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_194(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_195/7}).
-compile({nowarn_unused_function,  yeccpars2_195/7}).
yeccpars2_195(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_195_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_195(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_196/7}).
-compile({nowarn_unused_function,  yeccpars2_196/7}).
yeccpars2_196(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_196_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_197/7}).
-compile({nowarn_unused_function,  yeccpars2_197/7}).
yeccpars2_197(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_198/7}).
-compile({nowarn_unused_function,  yeccpars2_198/7}).
yeccpars2_198(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_199/7}).
-compile({nowarn_unused_function,  yeccpars2_199/7}).
yeccpars2_199(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_200/7}).
-compile({nowarn_unused_function,  yeccpars2_200/7}).
yeccpars2_200(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_201(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_202/7}).
-compile({nowarn_unused_function,  yeccpars2_202/7}).
yeccpars2_202(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_202_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_202(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_203/7}).
-compile({nowarn_unused_function,  yeccpars2_203/7}).
yeccpars2_203(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_203_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_203(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_204/7}).
-compile({nowarn_unused_function,  yeccpars2_204/7}).
yeccpars2_204(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_204_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_204(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_205/7}).
-compile({nowarn_unused_function,  yeccpars2_205/7}).
yeccpars2_205(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_205_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_and(Stack),
 yeccgoto_expr(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_cons(Stack),
 yeccgoto_expr(hd(Nss), 'cons', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'do', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_do(Stack),
 yeccgoto_expr(hd(Nss), 'do', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'fn', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_fn(Stack),
 yeccgoto_expr(hd(Nss), 'fn', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'handle', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_handle(Stack),
 yeccgoto_expr(hd(Nss), 'handle', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'instance', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_instance(Stack),
 yeccgoto_expr(hd(Nss), 'instance', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'match', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_match(Stack),
 yeccgoto_expr(hd(Nss), 'match', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_of(Stack),
 yeccgoto_expr(hd(Nss), 'of', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_or(Stack),
 yeccgoto_expr(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_semicolon(Stack),
 yeccgoto_expr(hd(Nss), 'semicolon', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_setoid_eq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_setoid_neq(Stack),
 yeccgoto_expr(hd(Nss), 'setoid_neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_trait(Stack),
 yeccgoto_expr(hd(Nss), 'trait', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_transform(Stack),
 yeccgoto_expr(hd(Nss), 'transform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'type', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_type(Stack),
 yeccgoto_expr(hd(Nss), 'type', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_205(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_206/7}).
-compile({nowarn_unused_function,  yeccpars2_206/7}).
yeccpars2_206(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_207(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_208/7}).
-compile({nowarn_unused_function,  yeccpars2_208/7}).
yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_209/7}).
-compile({nowarn_unused_function,  yeccpars2_209/7}).
yeccpars2_209(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_210/7}).
-compile({nowarn_unused_function,  yeccpars2_210/7}).
yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_perform_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_211/7}).
-compile({nowarn_unused_function,  yeccpars2_211/7}).
yeccpars2_211(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_212/7}).
-compile({nowarn_unused_function,  yeccpars2_212/7}).
yeccpars2_212(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_match_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_213(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_213(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_213/7}).
-compile({nowarn_unused_function,  yeccpars2_213/7}).
yeccpars2_cont_213(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_213(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_214: see yeccpars2_97

-dialyzer({nowarn_function, yeccpars2_215/7}).
-compile({nowarn_unused_function,  yeccpars2_215/7}).
yeccpars2_215(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_216: see yeccpars2_141

%% yeccpars2_217: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_218/7}).
-compile({nowarn_unused_function,  yeccpars2_218/7}).
yeccpars2_218(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_219: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_220/7}).
-compile({nowarn_unused_function,  yeccpars2_220/7}).
yeccpars2_220(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_221/7}).
-compile({nowarn_unused_function,  yeccpars2_221/7}).
yeccpars2_221(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_222/7}).
-compile({nowarn_unused_function,  yeccpars2_222/7}).
yeccpars2_222(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_223/7}).
-compile({nowarn_unused_function,  yeccpars2_223/7}).
yeccpars2_223(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_224/7}).
-compile({nowarn_unused_function,  yeccpars2_224/7}).
yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_225/7}).
-compile({nowarn_unused_function,  yeccpars2_225/7}).
yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_match_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_226/7}).
-compile({nowarn_unused_function,  yeccpars2_226/7}).
yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_227/7}).
-compile({nowarn_unused_function,  yeccpars2_227/7}).
yeccpars2_227(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_228(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_213(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_229: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_230/7}).
-compile({nowarn_unused_function,  yeccpars2_230/7}).
yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_231/7}).
-compile({nowarn_unused_function,  yeccpars2_231/7}).
yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_231_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_232/7}).
-compile({nowarn_unused_function,  yeccpars2_232/7}).
yeccpars2_232(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_233/7}).
-compile({nowarn_unused_function,  yeccpars2_233/7}).
yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_233_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_234/7}).
-compile({nowarn_unused_function,  yeccpars2_234/7}).
yeccpars2_234(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_235: see yeccpars2_141

yeccpars2_236(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_213(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_237: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_238/7}).
-compile({nowarn_unused_function,  yeccpars2_238/7}).
yeccpars2_238(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_238_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_239/7}).
-compile({nowarn_unused_function,  yeccpars2_239/7}).
yeccpars2_239(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_240/7}).
-compile({nowarn_unused_function,  yeccpars2_240/7}).
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_241/7}).
-compile({nowarn_unused_function,  yeccpars2_241/7}).
yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_242/7}).
-compile({nowarn_unused_function,  yeccpars2_242/7}).
yeccpars2_242(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_243/7}).
-compile({nowarn_unused_function,  yeccpars2_243/7}).
yeccpars2_243(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_244/7}).
-compile({nowarn_unused_function,  yeccpars2_244/7}).
yeccpars2_244(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_245/7}).
-compile({nowarn_unused_function,  yeccpars2_245/7}).
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_245_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_246: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_247/7}).
-compile({nowarn_unused_function,  yeccpars2_247/7}).
yeccpars2_247(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_248/7}).
-compile({nowarn_unused_function,  yeccpars2_248/7}).
yeccpars2_248(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_249/7}).
-compile({nowarn_unused_function,  yeccpars2_249/7}).
yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_249_(Stack),
 yeccgoto_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_250/7}).
-compile({nowarn_unused_function,  yeccpars2_250/7}).
yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_251(S, 'then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_213(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_252/7}).
-compile({nowarn_unused_function,  yeccpars2_252/7}).
yeccpars2_252(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_253/7}).
-compile({nowarn_unused_function,  yeccpars2_253/7}).
yeccpars2_253(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_254/7}).
-compile({nowarn_unused_function,  yeccpars2_254/7}).
yeccpars2_254(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_255/7}).
-compile({nowarn_unused_function,  yeccpars2_255/7}).
yeccpars2_255(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_255_(Stack),
 yeccgoto_handler_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_256/7}).
-compile({nowarn_unused_function,  yeccpars2_256/7}).
yeccpars2_256(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_257/7}).
-compile({nowarn_unused_function,  yeccpars2_257/7}).
yeccpars2_257(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_258/7}).
-compile({nowarn_unused_function,  yeccpars2_258/7}).
yeccpars2_258(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_259/7}).
-compile({nowarn_unused_function,  yeccpars2_259/7}).
yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_operation_cases(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_260/7}).
-compile({nowarn_unused_function,  yeccpars2_260/7}).
yeccpars2_260(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_261: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_262/7}).
-compile({nowarn_unused_function,  yeccpars2_262/7}).
yeccpars2_262(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_262_(Stack),
 yeccpars2_263(263, Cat, [262 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_263/7}).
-compile({nowarn_unused_function,  yeccpars2_263/7}).
yeccpars2_263(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_264/7}).
-compile({nowarn_unused_function,  yeccpars2_264/7}).
yeccpars2_264(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccgoto_pattern_list_comma(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_265/7}).
-compile({nowarn_unused_function,  yeccpars2_265/7}).
yeccpars2_265(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_265_(Stack),
 yeccpars2_266(_S, Cat, [265 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_266/7}).
-compile({nowarn_unused_function,  yeccpars2_266/7}).
yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 yeccgoto_pattern_list_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_267/7}).
-compile({nowarn_unused_function,  yeccpars2_267/7}).
yeccpars2_267(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_267(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_268: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_269/7}).
-compile({nowarn_unused_function,  yeccpars2_269/7}).
yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_269_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_270/7}).
-compile({nowarn_unused_function,  yeccpars2_270/7}).
yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_270_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_271/7}).
-compile({nowarn_unused_function,  yeccpars2_271/7}).
yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_271_(Stack),
 yeccgoto_operation_cases(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_272/7}).
-compile({nowarn_unused_function,  yeccpars2_272/7}).
yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_272_(Stack),
 yeccgoto_handler_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_273/7}).
-compile({nowarn_unused_function,  yeccpars2_273/7}).
yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_273_(Stack),
 yeccgoto_handler_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_274/7}).
-compile({nowarn_unused_function,  yeccpars2_274/7}).
yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_274_(Stack),
 yeccgoto_try_with_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_275/7}).
-compile({nowarn_unused_function,  yeccpars2_275/7}).
yeccpars2_275(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr);
yeccpars2_275(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_276: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_277/7}).
-compile({nowarn_unused_function,  yeccpars2_277/7}).
yeccpars2_277(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_277_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_278(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_141(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_279/7}).
-compile({nowarn_unused_function,  yeccpars2_279/7}).
yeccpars2_279(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_279_rbrace(Stack),
 yeccgoto_do_statements(hd(Ss), 'rbrace', Ss, NewStack, T, Ts, Tzr);
yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_do_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_280/7}).
-compile({nowarn_unused_function,  yeccpars2_280/7}).
yeccpars2_280(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_281/7}).
-compile({nowarn_unused_function,  yeccpars2_281/7}).
yeccpars2_281(S, 'semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_281(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_282/7}).
-compile({nowarn_unused_function,  yeccpars2_282/7}).
yeccpars2_282(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_282(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_283/7}).
-compile({nowarn_unused_function,  yeccpars2_283/7}).
yeccpars2_283(S, 'left_arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_283_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_284: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_285/7}).
-compile({nowarn_unused_function,  yeccpars2_285/7}).
yeccpars2_285(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_285_(Stack),
 yeccgoto_do_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_286/7}).
-compile({nowarn_unused_function,  yeccpars2_286/7}).
yeccpars2_286(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_287: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_288/7}).
-compile({nowarn_unused_function,  yeccpars2_288/7}).
yeccpars2_288(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_288_(Stack),
 yeccgoto_do_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_289: see yeccpars2_278

-dialyzer({nowarn_function, yeccpars2_290/7}).
-compile({nowarn_unused_function,  yeccpars2_290/7}).
yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_290_(Stack),
 yeccgoto_do_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_291/7}).
-compile({nowarn_unused_function,  yeccpars2_291/7}).
yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_291_(Stack),
 yeccgoto_do_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_292/7}).
-compile({nowarn_unused_function,  yeccpars2_292/7}).
yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_292_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_293/7}).
-compile({nowarn_unused_function,  yeccpars2_293/7}).
yeccpars2_293(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_293(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_294/7}).
-compile({nowarn_unused_function,  yeccpars2_294/7}).
yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_294_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_295: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_296/7}).
-compile({nowarn_unused_function,  yeccpars2_296/7}).
yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_296_(Stack),
 yeccgoto_guards(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_297: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_298/7}).
-compile({nowarn_unused_function,  yeccpars2_298/7}).
yeccpars2_298(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_298_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_299/7}).
-compile({nowarn_unused_function,  yeccpars2_299/7}).
yeccpars2_299(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_299_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_300: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_301/7}).
-compile({nowarn_unused_function,  yeccpars2_301/7}).
yeccpars2_301(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_301_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_302/7}).
-compile({nowarn_unused_function,  yeccpars2_302/7}).
yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_302_(Stack),
 yeccgoto_trait_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_303/7}).
-compile({nowarn_unused_function,  yeccpars2_303/7}).
yeccpars2_303(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_303_(Stack),
 yeccpars2_304(304, Cat, [303 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_304/7}).
-compile({nowarn_unused_function,  yeccpars2_304/7}).
yeccpars2_304(S, 'extend', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_304_(Stack),
 yeccpars2_305(305, Cat, [304 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_305/7}).
-compile({nowarn_unused_function,  yeccpars2_305/7}).
yeccpars2_305(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_305(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_306: see yeccpars2_50

-dialyzer({nowarn_function, yeccpars2_307/7}).
-compile({nowarn_unused_function,  yeccpars2_307/7}).
yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_307_(Stack),
 yeccgoto_maybe_trait_extends(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_308/7}).
-compile({nowarn_unused_function,  yeccpars2_308/7}).
yeccpars2_308(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_308_(Stack),
 yeccgoto_trait_extends_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_309: see yeccpars2_50

-dialyzer({nowarn_function, yeccpars2_310/7}).
-compile({nowarn_unused_function,  yeccpars2_310/7}).
yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_310_(Stack),
 yeccgoto_trait_extends_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_311/7}).
-compile({nowarn_unused_function,  yeccpars2_311/7}).
yeccpars2_311(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr);
yeccpars2_311(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_312/7}).
-compile({nowarn_unused_function,  yeccpars2_312/7}).
yeccpars2_312(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_313/7}).
-compile({nowarn_unused_function,  yeccpars2_313/7}).
yeccpars2_313(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr);
yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_313_(Stack),
 yeccgoto_trait_members(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_314(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr);
yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_315/7}).
-compile({nowarn_unused_function,  yeccpars2_315/7}).
yeccpars2_315(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr);
yeccpars2_315(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_316(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_14(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_317/7}).
-compile({nowarn_unused_function,  yeccpars2_317/7}).
yeccpars2_317(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_317_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_318/7}).
-compile({nowarn_unused_function,  yeccpars2_318/7}).
yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_318_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_319: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_320/7}).
-compile({nowarn_unused_function,  yeccpars2_320/7}).
yeccpars2_320(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_320_(Stack),
 yeccgoto_trait_member(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_321/7}).
-compile({nowarn_unused_function,  yeccpars2_321/7}).
yeccpars2_321(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_321_(Stack),
 yeccgoto_trait_members(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_322/7}).
-compile({nowarn_unused_function,  yeccpars2_322/7}).
yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_322_(Stack),
 yeccgoto_trait_members(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_323/7}).
-compile({nowarn_unused_function,  yeccpars2_323/7}).
yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_323_(Stack),
 yeccgoto_trait_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_324/7}).
-compile({nowarn_unused_function,  yeccpars2_324/7}).
yeccpars2_324(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 325, Ss, Stack, T, Ts, Tzr);
yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_324_(Stack),
 yeccgoto_module_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_325/7}).
-compile({nowarn_unused_function,  yeccpars2_325/7}).
yeccpars2_325(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 326, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_326/7}).
-compile({nowarn_unused_function,  yeccpars2_326/7}).
yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_326_(Stack),
 yeccgoto_module_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_327/7}).
-compile({nowarn_unused_function,  yeccpars2_327/7}).
yeccpars2_327(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_327_(Stack),
 yeccgoto_instance_constraints(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_328/7}).
-compile({nowarn_unused_function,  yeccpars2_328/7}).
yeccpars2_328(S, 'double_arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 346, Ss, Stack, T, Ts, Tzr);
yeccpars2_328(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_329/7}).
-compile({nowarn_unused_function,  yeccpars2_329/7}).
yeccpars2_329(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_329_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_330/7}).
-compile({nowarn_unused_function,  yeccpars2_330/7}).
yeccpars2_330(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_330_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_331/7}).
-compile({nowarn_unused_function,  yeccpars2_331/7}).
yeccpars2_331(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_331(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_331(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_331(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_331(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_comma(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), 'comma', Ss, NewStack, T, Ts, Tzr);
yeccpars2_331(_S, 'double_arrow', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_double_arrow(Stack),
 yeccgoto_type_expr_primary_list(hd(Ss), 'double_arrow', Ss, NewStack, T, Ts, Tzr);
yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_(Stack),
 yeccgoto_instance_type_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_332/7}).
-compile({nowarn_unused_function,  yeccpars2_332/7}).
yeccpars2_332(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 333, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_333/7}).
-compile({nowarn_unused_function,  yeccpars2_333/7}).
yeccpars2_333(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 336, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_334/7}).
-compile({nowarn_unused_function,  yeccpars2_334/7}).
yeccpars2_334(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_335/7}).
-compile({nowarn_unused_function,  yeccpars2_335/7}).
yeccpars2_335(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr);
yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_335_(Stack),
 yeccgoto_instance_methods(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_336/7}).
-compile({nowarn_unused_function,  yeccpars2_336/7}).
yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_336_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_337/7}).
-compile({nowarn_unused_function,  yeccpars2_337/7}).
yeccpars2_337(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_338/7}).
-compile({nowarn_unused_function,  yeccpars2_338/7}).
yeccpars2_338(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_338_(Stack),
 yeccpars2_339(339, Cat, [338 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_339/7}).
-compile({nowarn_unused_function,  yeccpars2_339/7}).
yeccpars2_339(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 340, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_340: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_341/7}).
-compile({nowarn_unused_function,  yeccpars2_341/7}).
yeccpars2_341(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_341_(Stack),
 yeccgoto_instance_method(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_342/7}).
-compile({nowarn_unused_function,  yeccpars2_342/7}).
yeccpars2_342(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_342_(Stack),
 yeccgoto_instance_methods(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_343/7}).
-compile({nowarn_unused_function,  yeccpars2_343/7}).
yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_343_(Stack),
 yeccgoto_instance_methods(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_344/7}).
-compile({nowarn_unused_function,  yeccpars2_344/7}).
yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_344_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_345/7}).
-compile({nowarn_unused_function,  yeccpars2_345/7}).
yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_345_(Stack),
 yeccgoto_instance_type_args(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_346/7}).
-compile({nowarn_unused_function,  yeccpars2_346/7}).
yeccpars2_346(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr);
yeccpars2_346(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_347(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_347(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_14(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_348/7}).
-compile({nowarn_unused_function,  yeccpars2_348/7}).
yeccpars2_348(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_348_(Stack),
 yeccgoto_instance_type_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_349/7}).
-compile({nowarn_unused_function,  yeccpars2_349/7}).
yeccpars2_349(S, 'where', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 350, Ss, Stack, T, Ts, Tzr);
yeccpars2_349(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_350/7}).
-compile({nowarn_unused_function,  yeccpars2_350/7}).
yeccpars2_350(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 352, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_351/7}).
-compile({nowarn_unused_function,  yeccpars2_351/7}).
yeccpars2_351(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_352/7}).
-compile({nowarn_unused_function,  yeccpars2_352/7}).
yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_352_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_353/7}).
-compile({nowarn_unused_function,  yeccpars2_353/7}).
yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_353_(Stack),
 yeccgoto_instance_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_354: see yeccpars2_50

-dialyzer({nowarn_function, yeccpars2_355/7}).
-compile({nowarn_unused_function,  yeccpars2_355/7}).
yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_355_(Stack),
 yeccgoto_instance_constraints(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_356/7}).
-compile({nowarn_unused_function,  yeccpars2_356/7}).
yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_356_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_357/7}).
-compile({nowarn_unused_function,  yeccpars2_357/7}).
yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_357_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_358/7}).
-compile({nowarn_unused_function,  yeccpars2_358/7}).
yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_358_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_359/7}).
-compile({nowarn_unused_function,  yeccpars2_359/7}).
yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_359_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_360/7}).
-compile({nowarn_unused_function,  yeccpars2_360/7}).
yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_360_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_361/7}).
-compile({nowarn_unused_function,  yeccpars2_361/7}).
yeccpars2_361(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_361_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_362/7}).
-compile({nowarn_unused_function,  yeccpars2_362/7}).
yeccpars2_362(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_362_(Stack),
 yeccpars2_363(363, Cat, [362 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_363/7}).
-compile({nowarn_unused_function,  yeccpars2_363/7}).
yeccpars2_363(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr);
yeccpars2_363(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_364/7}).
-compile({nowarn_unused_function,  yeccpars2_364/7}).
yeccpars2_364(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_364_(Stack),
 yeccpars2_369(_S, Cat, [364 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_365/7}).
-compile({nowarn_unused_function,  yeccpars2_365/7}).
yeccpars2_365(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_366/7}).
-compile({nowarn_unused_function,  yeccpars2_366/7}).
yeccpars2_366(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr);
yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_366_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_367: see yeccpars2_34

-dialyzer({nowarn_function, yeccpars2_368/7}).
-compile({nowarn_unused_function,  yeccpars2_368/7}).
yeccpars2_368(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(S, 'constrain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_368_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_369/7}).
-compile({nowarn_unused_function,  yeccpars2_369/7}).
yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_369_(Stack),
 yeccgoto_effect_operations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_370/7}).
-compile({nowarn_unused_function,  yeccpars2_370/7}).
yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_370_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_371/7}).
-compile({nowarn_unused_function,  yeccpars2_371/7}).
yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_371_(Stack),
 yeccgoto_declarations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_372/7}).
-compile({nowarn_unused_function,  yeccpars2_372/7}).
yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_372_(Stack),
 yeccgoto_export_decl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_373/7}).
-compile({nowarn_unused_function,  yeccpars2_373/7}).
yeccpars2_373(S, 'export', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_373_(Stack),
 yeccgoto_export_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_374/7}).
-compile({nowarn_unused_function,  yeccpars2_374/7}).
yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_374_(Stack),
 yeccgoto_module_header(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_375/7}).
-compile({nowarn_unused_function,  yeccpars2_375/7}).
yeccpars2_375(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(S, 'trait', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(S, 'type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_376/7}).
-compile({nowarn_unused_function,  yeccpars2_376/7}).
yeccpars2_376(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 383, Ss, Stack, T, Ts, Tzr);
yeccpars2_376(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_377/7}).
-compile({nowarn_unused_function,  yeccpars2_377/7}).
yeccpars2_377(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 382, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_378/7}).
-compile({nowarn_unused_function,  yeccpars2_378/7}).
yeccpars2_378(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr);
yeccpars2_378(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_379/7}).
-compile({nowarn_unused_function,  yeccpars2_379/7}).
yeccpars2_379(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr);
yeccpars2_379(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_380/7}).
-compile({nowarn_unused_function,  yeccpars2_380/7}).
yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_380_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_381/7}).
-compile({nowarn_unused_function,  yeccpars2_381/7}).
yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_381_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_382/7}).
-compile({nowarn_unused_function,  yeccpars2_382/7}).
yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_382_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_383/7}).
-compile({nowarn_unused_function,  yeccpars2_383/7}).
yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_export_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_384/7}).
-compile({nowarn_unused_function,  yeccpars2_384/7}).
yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_384_(Stack),
 yeccgoto_export_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_385/7}).
-compile({nowarn_unused_function,  yeccpars2_385/7}).
yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_385_(Stack),
 yeccgoto_catena_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_386/7}).
-compile({nowarn_unused_function,  yeccpars2_386/7}).
yeccpars2_386(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_386_(Stack),
 yeccgoto_transform_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_387/7}).
-compile({nowarn_unused_function,  yeccpars2_387/7}).
yeccpars2_387(S, 'transform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_387_(Stack),
 yeccgoto_transform_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_388/7}).
-compile({nowarn_unused_function,  yeccpars2_388/7}).
yeccpars2_388(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_389/7}).
-compile({nowarn_unused_function,  yeccpars2_389/7}).
yeccpars2_389(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_389_(Stack),
 yeccpars2_390(390, Cat, [389 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_390/7}).
-compile({nowarn_unused_function,  yeccpars2_390/7}).
yeccpars2_390(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 391, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_391: see yeccpars2_141

%% yeccpars2_392: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_393/7}).
-compile({nowarn_unused_function,  yeccpars2_393/7}).
yeccpars2_393(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 394, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_394: see yeccpars2_141

-dialyzer({nowarn_function, yeccpars2_395/7}).
-compile({nowarn_unused_function,  yeccpars2_395/7}).
yeccpars2_395(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_395_(Stack),
 yeccgoto_transform_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_396/7}).
-compile({nowarn_unused_function,  yeccpars2_396/7}).
yeccpars2_396(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'cons', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'plus_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'setoid_eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'setoid_neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_396_(Stack),
 yeccgoto_transform_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_397/7}).
-compile({nowarn_unused_function,  yeccpars2_397/7}).
yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_397_(Stack),
 yeccgoto_transform_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_catena_module/7}).
-compile({nowarn_unused_function,  yeccgoto_catena_module/7}).
yeccgoto_catena_module(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor/7}).
yeccgoto_constructor(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(84, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor_fields/7}).
yeccgoto_constructor_fields(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor_fields(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructors/7}).
-compile({nowarn_unused_function,  yeccgoto_constructors/7}).
yeccgoto_constructors(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(82=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(84=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declaration/7}).
-compile({nowarn_unused_function,  yeccgoto_declaration/7}).
yeccgoto_declaration(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarations/7}).
-compile({nowarn_unused_function,  yeccgoto_declarations/7}).
yeccgoto_declarations(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_do_expr/7}).
yeccgoto_do_expr(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(229=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(261=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(284=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(300=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(391=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_expr(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_statement/7}).
-compile({nowarn_unused_function,  yeccgoto_do_statement/7}).
yeccgoto_do_statement(278, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(281, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_statement(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(281, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_do_statements/7}).
-compile({nowarn_unused_function,  yeccgoto_do_statements/7}).
yeccgoto_do_statements(278, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(280, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_do_statements(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_decl/7}).
yeccgoto_effect_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_decl(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_decl(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_list_nonempty/7}).
yeccgoto_effect_list_nonempty(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_list_nonempty(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operation/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operation/7}).
yeccgoto_effect_operation(362, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(364, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operation(364, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(364, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operations/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operations/7}).
yeccgoto_effect_operations(362, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(363, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operations(364=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_export_decl/7}).
yeccgoto_export_decl(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_item/7}).
-compile({nowarn_unused_function,  yeccgoto_export_item/7}).
yeccgoto_export_item(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(373, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_export_item(373, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(373, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_list/7}).
-compile({nowarn_unused_function,  yeccgoto_export_list/7}).
yeccgoto_export_list(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_export_list(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_expr/7}).
yeccgoto_expr(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(299, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(251, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(175, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(202, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(199, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(197, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(196, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(195, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(194, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(220, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(229, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(236, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(247, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(277, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(278, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(279, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(284, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(285, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(288, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(279, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(297, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(298, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(300, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(301, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(320, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(341, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(391, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(396, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(392, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(395, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_app/7}).
yeccgoto_expr_app(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(175, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(229, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(278, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(284, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(297, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(300, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(391, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(392, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list/7}).
yeccgoto_expr_list(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(239, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list_opt/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list_opt/7}).
yeccgoto_expr_list_opt(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(171, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_primary/7}).
yeccgoto_expr_primary(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(229=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(261=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(284=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(300=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(391=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guard/7}).
-compile({nowarn_unused_function,  yeccgoto_guard/7}).
yeccgoto_guard(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(392, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guards/7}).
-compile({nowarn_unused_function,  yeccgoto_guards/7}).
yeccgoto_guards(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(392, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(393, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clause/7}).
yeccgoto_handler_clause(253, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(255, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clause(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(255, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clauses/7}).
yeccgoto_handler_clauses(253, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(254, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clauses(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_constraints/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_constraints/7}).
yeccgoto_instance_constraints(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(328, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_constraints(354=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_decl/7}).
yeccgoto_instance_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_decl(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_decl(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_method/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_method/7}).
yeccgoto_instance_method(333, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(335, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_method(342, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(335, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_method(350, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(335, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_methods/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_methods/7}).
yeccgoto_instance_methods(333, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(334, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_methods(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_methods(350, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(351, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_instance_type_args/7}).
-compile({nowarn_unused_function,  yeccgoto_instance_type_args/7}).
yeccgoto_instance_type_args(330, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(332, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(331=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(347, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_instance_type_args(348=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
-compile({nowarn_unused_function,  yeccgoto_literal/7}).
yeccgoto_literal(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(229=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(261=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(284=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(300=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(391=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clause/7}).
yeccgoto_match_clause(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(222, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clauses/7}).
yeccgoto_match_clauses(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(212=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(222, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_maybe_trait_extends/7}).
-compile({nowarn_unused_function,  yeccgoto_maybe_trait_extends/7}).
yeccgoto_maybe_trait_extends(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(305, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_module_decl/7}).
yeccgoto_module_decl(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module_header/7}).
-compile({nowarn_unused_function,  yeccgoto_module_header/7}).
yeccgoto_module_header(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_case/7}).
-compile({nowarn_unused_function,  yeccgoto_operation_case/7}).
yeccgoto_operation_case(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operation_case(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_cases/7}).
-compile({nowarn_unused_function,  yeccgoto_operation_cases/7}).
yeccgoto_operation_cases(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(258, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern/7}).
yeccgoto_pattern(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(126, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(136, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(338, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list/7}).
yeccgoto_pattern_list(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(338, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(339, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(390, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_comma/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_comma/7}).
yeccgoto_pattern_list_comma(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_comma(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_nonempty/7}).
yeccgoto_pattern_list_nonempty(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(104=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(315, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(389=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_primary/7}).
yeccgoto_pattern_primary(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_primary(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_perform_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_perform_expr/7}).
yeccgoto_perform_expr(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(229=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(261=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(284=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(300=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(391=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_field/7}).
yeccgoto_record_field(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_field(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_fields/7}).
yeccgoto_record_fields(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(242, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_fields(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_field/7}).
yeccgoto_record_pattern_field(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_field(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_fields/7}).
yeccgoto_record_pattern_fields(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_fields(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_constraint/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_constraint/7}).
yeccgoto_trait_constraint(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(327, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(306, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(309, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_constraint(354, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(327, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_decl/7}).
yeccgoto_trait_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_decl(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_decl(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_extends_list/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_extends_list/7}).
yeccgoto_trait_extends_list(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_extends_list(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_member/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_member/7}).
yeccgoto_trait_member(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(313, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_member(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(313, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_trait_members/7}).
-compile({nowarn_unused_function,  yeccgoto_trait_members/7}).
yeccgoto_trait_members(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(312, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_trait_members(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_clause/7}).
yeccgoto_transform_clause(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(387, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_clause(387, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(387, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_clauses/7}).
yeccgoto_transform_clauses(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_clauses(387=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_decl/7}).
yeccgoto_transform_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_decl(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_decl(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_transform_signature/7}).
-compile({nowarn_unused_function,  yeccgoto_transform_signature/7}).
yeccgoto_transform_signature(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_signature(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transform_signature(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_try_with_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_try_with_expr/7}).
yeccgoto_try_with_expr(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(229=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(261=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(284=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(300=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(391=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_expr_list/7}).
yeccgoto_tuple_expr_list(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_expr_list(229=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_pattern_list/7}).
yeccgoto_tuple_pattern_list(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_pattern_list(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_constraints/7}).
-compile({nowarn_unused_function,  yeccgoto_type_constraints/7}).
yeccgoto_type_constraints(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_constraints(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_type_decl/7}).
yeccgoto_type_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_decl(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_decl(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr/7}).
yeccgoto_type_expr(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(317, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(367, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(368, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_app/7}).
yeccgoto_type_expr_app(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(354=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(367, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_list/7}).
yeccgoto_type_expr_list(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_list(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary/7}).
yeccgoto_type_expr_primary(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(306=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(330, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(331, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(331, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(331, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(347, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(348, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(354=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary_list/7}).
yeccgoto_type_expr_primary_list(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(330=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary_list(331=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params/7}).
yeccgoto_type_params(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(22, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(303, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(304, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params_nonempty/7}).
yeccgoto_type_params_nonempty(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_field/7}).
yeccgoto_type_record_field(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_field(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_fields/7}).
yeccgoto_type_record_fields(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_fields(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-dialyzer({nowarn_function, yeccpars2_1_/1}).
-compile({nowarn_unused_function,  yeccpars2_1_/1}).
-file("src/compiler/parser/catena_parser.yrl", 245).
yeccpars2_1_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_2_/1}).
-dialyzer({nowarn_function, yeccpars2_2_/1}).
-compile({nowarn_unused_function,  yeccpars2_2_/1}).
-file("src/compiler/parser/catena_parser.yrl", 486).
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
-file("src/compiler/parser/catena_parser.yrl", 246).
yeccpars2_3_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                ___1
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function,  yeccpars2_4_/1}).
-file("src/compiler/parser/catena_parser.yrl", 248).
yeccpars2_4_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            ___1
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function,  yeccpars2_6_/1}).
-file("src/compiler/parser/catena_parser.yrl", 205).
yeccpars2_6_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              
    {module_header, element(2, ___1), element(3, ___1), element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-dialyzer({nowarn_function, yeccpars2_7_/1}).
-compile({nowarn_unused_function,  yeccpars2_7_/1}).
-file("src/compiler/parser/catena_parser.yrl", 249).
yeccpars2_7_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-dialyzer({nowarn_function, yeccpars2_8_/1}).
-compile({nowarn_unused_function,  yeccpars2_8_/1}).
-file("src/compiler/parser/catena_parser.yrl", 247).
yeccpars2_8_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function,  yeccpars2_9_/1}).
-file("src/compiler/parser/catena_parser.yrl", 201).
yeccpars2_9_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    {module, undefined, [], [], ___1, {line, 1}}
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function,  yeccpars2_10_/1}).
-file("src/compiler/parser/catena_parser.yrl", 240).
yeccpars2_10_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-dialyzer({nowarn_function, yeccpars2_19_/1}).
-compile({nowarn_unused_function,  yeccpars2_19_/1}).
-file("src/compiler/parser/catena_parser.yrl", 280).
yeccpars2_19_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                         
    make_error_declaration(extract_location(___1), "Incomplete type declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-dialyzer({nowarn_function, yeccpars2_20_/1}).
-compile({nowarn_unused_function,  yeccpars2_20_/1}).
-file("src/compiler/parser/catena_parser.yrl", 283).
yeccpars2_20_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_21_/1}).
-dialyzer({nowarn_function, yeccpars2_21_/1}).
-compile({nowarn_unused_function,  yeccpars2_21_/1}).
-file("src/compiler/parser/catena_parser.yrl", 285).
yeccpars2_21_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    ___1
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function,  yeccpars2_23_/1}).
-file("src/compiler/parser/catena_parser.yrl", 288).
yeccpars2_23_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-dialyzer({nowarn_function, yeccpars2_24_/1}).
-compile({nowarn_unused_function,  yeccpars2_24_/1}).
-file("src/compiler/parser/catena_parser.yrl", 290).
yeccpars2_24_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    [extract_atom(___1) | ___2]
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-dialyzer({nowarn_function, yeccpars2_26_/1}).
-compile({nowarn_unused_function,  yeccpars2_26_/1}).
-file("src/compiler/parser/catena_parser.yrl", 278).
yeccpars2_26_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                 
    make_error_declaration(extract_location(___1), "Missing '=' or constructors in type declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-dialyzer({nowarn_function, yeccpars2_27_/1}).
-compile({nowarn_unused_function,  yeccpars2_27_/1}).
-file("src/compiler/parser/catena_parser.yrl", 267).
yeccpars2_27_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    {type_decl,
        extract_atom(___2),
        ___3,
        ___5,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-dialyzer({nowarn_function, yeccpars2_28_/1}).
-compile({nowarn_unused_function,  yeccpars2_28_/1}).
-file("src/compiler/parser/catena_parser.yrl", 293).
yeccpars2_28_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-dialyzer({nowarn_function, yeccpars2_29_/1}).
-compile({nowarn_unused_function,  yeccpars2_29_/1}).
-file("src/compiler/parser/catena_parser.yrl", 298).
yeccpars2_29_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {constructor,
        extract_atom(___1),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function,  yeccpars2_30_/1}).
-file("src/compiler/parser/catena_parser.yrl", 310).
yeccpars2_30_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-dialyzer({nowarn_function, yeccpars2_31_/1}).
-compile({nowarn_unused_function,  yeccpars2_31_/1}).
-file("src/compiler/parser/catena_parser.yrl", 304).
yeccpars2_31_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {constructor,
        extract_atom(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-dialyzer({nowarn_function, yeccpars2_33_/1}).
-compile({nowarn_unused_function,  yeccpars2_33_/1}).
-file("src/compiler/parser/catena_parser.yrl", 990).
yeccpars2_33_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function,  yeccpars2_35_/1}).
-file("src/compiler/parser/catena_parser.yrl", 993).
yeccpars2_35_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-dialyzer({nowarn_function, yeccpars2_36_/1}).
-compile({nowarn_unused_function,  yeccpars2_36_/1}).
-file("src/compiler/parser/catena_parser.yrl", 979).
yeccpars2_36_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                    
    ___1
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-dialyzer({nowarn_function, yeccpars2_37_/1}).
-compile({nowarn_unused_function,  yeccpars2_37_/1}).
-file("src/compiler/parser/catena_parser.yrl", 953).
yeccpars2_37_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function,  yeccpars2_39_/1}).
-file("src/compiler/parser/catena_parser.yrl", 283).
yeccpars2_39_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function,  yeccpars2_40_/1}).
-file("src/compiler/parser/catena_parser.yrl", 990).
yeccpars2_40_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-dialyzer({nowarn_function, yeccpars2_41_/1}).
-compile({nowarn_unused_function,  yeccpars2_41_/1}).
-file("src/compiler/parser/catena_parser.yrl", 993).
yeccpars2_41_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-dialyzer({nowarn_function, yeccpars2_42_/1}).
-compile({nowarn_unused_function,  yeccpars2_42_/1}).
-file("src/compiler/parser/catena_parser.yrl", 966).
yeccpars2_42_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {type_app,
        {type_con, extract_atom(___1), extract_location(___1)},
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function,  yeccpars2_43_/1}).
-file("src/compiler/parser/catena_parser.yrl", 984).
yeccpars2_43_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-dialyzer({nowarn_function, yeccpars2_44_/1}).
-compile({nowarn_unused_function,  yeccpars2_44_/1}).
-file("src/compiler/parser/catena_parser.yrl", 986).
yeccpars2_44_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                                    
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-dialyzer({nowarn_function, yeccpars2_45_/1}).
-compile({nowarn_unused_function,  yeccpars2_45_/1}).
-file("src/compiler/parser/catena_parser.yrl", 973).
yeccpars2_45_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {type_app,
        {type_var, extract_atom(___1), extract_location(___1)},
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-dialyzer({nowarn_function, yeccpars2_48_/1}).
-compile({nowarn_unused_function,  yeccpars2_48_/1}).
-file("src/compiler/parser/catena_parser.yrl", 940).
yeccpars2_48_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {type_forall, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-dialyzer({nowarn_function, yeccpars2_51_/1}).
-compile({nowarn_unused_function,  yeccpars2_51_/1}).
-file("src/compiler/parser/catena_parser.yrl", 376).
yeccpars2_51_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    extract_trait_constraint(___1)
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-dialyzer({nowarn_function, yeccpars2_52_/1}).
-compile({nowarn_unused_function,  yeccpars2_52_/1}).
-file("src/compiler/parser/catena_parser.yrl", 950).
yeccpars2_52_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {constrained_type, ___3, ___1, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-dialyzer({nowarn_function, yeccpars2_53_/1}).
-compile({nowarn_unused_function,  yeccpars2_53_/1}).
-file("src/compiler/parser/catena_parser.yrl", 956).
yeccpars2_53_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                      
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-dialyzer({nowarn_function, yeccpars2_55_/1}).
-compile({nowarn_unused_function,  yeccpars2_55_/1}).
-file("src/compiler/parser/catena_parser.yrl", 958).
yeccpars2_55_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-dialyzer({nowarn_function, yeccpars2_56_/1}).
-compile({nowarn_unused_function,  yeccpars2_56_/1}).
-file("src/compiler/parser/catena_parser.yrl", 937).
yeccpars2_56_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {type_fun, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-dialyzer({nowarn_function, yeccpars2_58_/1}).
-compile({nowarn_unused_function,  yeccpars2_58_/1}).
-file("src/compiler/parser/catena_parser.yrl", 997).
yeccpars2_58_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              
    ___2
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-dialyzer({nowarn_function, yeccpars2_60_/1}).
-compile({nowarn_unused_function,  yeccpars2_60_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1012).
yeccpars2_60_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-dialyzer({nowarn_function, yeccpars2_62_/1}).
-compile({nowarn_unused_function,  yeccpars2_62_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1014).
yeccpars2_62_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-dialyzer({nowarn_function, yeccpars2_63_/1}).
-compile({nowarn_unused_function,  yeccpars2_63_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1002).
yeccpars2_63_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   
    {type_tuple, [___2 | ___4], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-dialyzer({nowarn_function, yeccpars2_67_/1}).
-compile({nowarn_unused_function,  yeccpars2_67_/1}).
-file("src/compiler/parser/catena_parser.yrl", 943).
yeccpars2_67_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {type_effect, ___1, [], extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-dialyzer({nowarn_function, yeccpars2_68_/1}).
-compile({nowarn_unused_function,  yeccpars2_68_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1038).
yeccpars2_68_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-dialyzer({nowarn_function, yeccpars2_70_/1}).
-compile({nowarn_unused_function,  yeccpars2_70_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1040).
yeccpars2_70_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                
    [extract_atom(___1) | ___3]
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-dialyzer({nowarn_function, yeccpars2_71_/1}).
-compile({nowarn_unused_function,  yeccpars2_71_/1}).
-file("src/compiler/parser/catena_parser.yrl", 946).
yeccpars2_71_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     
    {type_effect, ___1, ___4, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-dialyzer({nowarn_function, yeccpars2_73_/1}).
-compile({nowarn_unused_function,  yeccpars2_73_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1024).
yeccpars2_73_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-dialyzer({nowarn_function, yeccpars2_75_/1}).
-compile({nowarn_unused_function,  yeccpars2_75_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1005).
yeccpars2_75_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    {type_record, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-dialyzer({nowarn_function, yeccpars2_77_/1}).
-compile({nowarn_unused_function,  yeccpars2_77_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1029).
yeccpars2_77_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-dialyzer({nowarn_function, yeccpars2_79_/1}).
-compile({nowarn_unused_function,  yeccpars2_79_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1026).
yeccpars2_79_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-dialyzer({nowarn_function, yeccpars2_80_/1}).
-compile({nowarn_unused_function,  yeccpars2_80_/1}).
-file("src/compiler/parser/catena_parser.yrl", 1008).
yeccpars2_80_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {type_record, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-dialyzer({nowarn_function, yeccpars2_81_/1}).
-compile({nowarn_unused_function,  yeccpars2_81_/1}).
-file("src/compiler/parser/catena_parser.yrl", 312).
yeccpars2_81_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-dialyzer({nowarn_function, yeccpars2_83_/1}).
-compile({nowarn_unused_function,  yeccpars2_83_/1}).
-file("src/compiler/parser/catena_parser.yrl", 295).
yeccpars2_83_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-dialyzer({nowarn_function, yeccpars2_85_/1}).
-compile({nowarn_unused_function,  yeccpars2_85_/1}).
-file("src/compiler/parser/catena_parser.yrl", 276).
yeccpars2_85_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    make_error_declaration(extract_location(___1), "Invalid type name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-dialyzer({nowarn_function, yeccpars2_86_/1}).
-compile({nowarn_unused_function,  yeccpars2_86_/1}).
-file("src/compiler/parser/catena_parser.yrl", 513).
yeccpars2_86_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    make_error_declaration(extract_location(___1), "Incomplete transform declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-dialyzer({nowarn_function, yeccpars2_87_/1}).
-compile({nowarn_unused_function,  yeccpars2_87_/1}).
-file("src/compiler/parser/catena_parser.yrl", 567).
yeccpars2_87_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_88_/1}).
-dialyzer({nowarn_function, yeccpars2_88_/1}).
-compile({nowarn_unused_function,  yeccpars2_88_/1}).
-file("src/compiler/parser/catena_parser.yrl", 569).
yeccpars2_88_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    ___1
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-dialyzer({nowarn_function, yeccpars2_90_/1}).
-compile({nowarn_unused_function,  yeccpars2_90_/1}).
-file("src/compiler/parser/catena_parser.yrl", 572).
yeccpars2_90_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-dialyzer({nowarn_function, yeccpars2_92_/1}).
-compile({nowarn_unused_function,  yeccpars2_92_/1}).
-file("src/compiler/parser/catena_parser.yrl", 632).
yeccpars2_92_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-dialyzer({nowarn_function, yeccpars2_93_/1}).
-compile({nowarn_unused_function,  yeccpars2_93_/1}).
-file("src/compiler/parser/catena_parser.yrl", 629).
yeccpars2_93_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-dialyzer({nowarn_function, yeccpars2_96_/1}).
-compile({nowarn_unused_function,  yeccpars2_96_/1}).
-file("src/compiler/parser/catena_parser.yrl", 585).
yeccpars2_96_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-dialyzer({nowarn_function, yeccpars2_98_/1}).
-compile({nowarn_unused_function,  yeccpars2_98_/1}).
-file("src/compiler/parser/catena_parser.yrl", 635).
yeccpars2_98_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-dialyzer({nowarn_function, yeccpars2_99_/1}).
-compile({nowarn_unused_function,  yeccpars2_99_/1}).
-file("src/compiler/parser/catena_parser.yrl", 588).
yeccpars2_99_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-dialyzer({nowarn_function, yeccpars2_100_/1}).
-compile({nowarn_unused_function,  yeccpars2_100_/1}).
-file("src/compiler/parser/catena_parser.yrl", 591).
yeccpars2_100_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_constructor, extract_atom(___1), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-dialyzer({nowarn_function, yeccpars2_101_/1}).
-compile({nowarn_unused_function,  yeccpars2_101_/1}).
-file("src/compiler/parser/catena_parser.yrl", 598).
yeccpars2_101_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {pat_constructor, extract_atom(___1), [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-dialyzer({nowarn_function, yeccpars2_102_/1}).
-compile({nowarn_unused_function,  yeccpars2_102_/1}).
-file("src/compiler/parser/catena_parser.yrl", 615).
yeccpars2_102_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-dialyzer({nowarn_function, yeccpars2_103_/1}).
-compile({nowarn_unused_function,  yeccpars2_103_/1}).
-file("src/compiler/parser/catena_parser.yrl", 612).
yeccpars2_103_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-dialyzer({nowarn_function, yeccpars2_105_/1}).
-compile({nowarn_unused_function,  yeccpars2_105_/1}).
-file("src/compiler/parser/catena_parser.yrl", 606).
yeccpars2_105_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-dialyzer({nowarn_function, yeccpars2_106_/1}).
-compile({nowarn_unused_function,  yeccpars2_106_/1}).
-file("src/compiler/parser/catena_parser.yrl", 567).
yeccpars2_106_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_107_/1}).
-dialyzer({nowarn_function, yeccpars2_107_/1}).
-compile({nowarn_unused_function,  yeccpars2_107_/1}).
-file("src/compiler/parser/catena_parser.yrl", 618).
yeccpars2_107_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-dialyzer({nowarn_function, yeccpars2_108_/1}).
-compile({nowarn_unused_function,  yeccpars2_108_/1}).
-file("src/compiler/parser/catena_parser.yrl", 609).
yeccpars2_108_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-dialyzer({nowarn_function, yeccpars2_111_/1}).
-compile({nowarn_unused_function,  yeccpars2_111_/1}).
-file("src/compiler/parser/catena_parser.yrl", 574).
yeccpars2_111_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-dialyzer({nowarn_function, yeccpars2_113_/1}).
-compile({nowarn_unused_function,  yeccpars2_113_/1}).
-file("src/compiler/parser/catena_parser.yrl", 621).
yeccpars2_113_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           ___2
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-dialyzer({nowarn_function, yeccpars2_114_/1}).
-compile({nowarn_unused_function,  yeccpars2_114_/1}).
-file("src/compiler/parser/catena_parser.yrl", 654).
yeccpars2_114_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    {pat_cons, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-dialyzer({nowarn_function, yeccpars2_115_/1}).
-compile({nowarn_unused_function,  yeccpars2_115_/1}).
-file("src/compiler/parser/catena_parser.yrl", 594).
yeccpars2_115_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_constructor, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-dialyzer({nowarn_function, yeccpars2_117_/1}).
-compile({nowarn_unused_function,  yeccpars2_117_/1}).
-file("src/compiler/parser/catena_parser.yrl", 623).
yeccpars2_117_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_118_/1}).
-dialyzer({nowarn_function, yeccpars2_118_/1}).
-compile({nowarn_unused_function,  yeccpars2_118_/1}).
-file("src/compiler/parser/catena_parser.yrl", 626).
yeccpars2_118_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-dialyzer({nowarn_function, yeccpars2_119_/1}).
-compile({nowarn_unused_function,  yeccpars2_119_/1}).
-file("src/compiler/parser/catena_parser.yrl", 602).
yeccpars2_119_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    {pat_constructor, extract_atom(___1), [___2, ___3], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-dialyzer({nowarn_function, yeccpars2_125_/1}).
-compile({nowarn_unused_function,  yeccpars2_125_/1}).
-file("src/compiler/parser/catena_parser.yrl", 660).
yeccpars2_125_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-dialyzer({nowarn_function, yeccpars2_126_/1}).
-compile({nowarn_unused_function,  yeccpars2_126_/1}).
-file("src/compiler/parser/catena_parser.yrl", 658).
yeccpars2_126_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-dialyzer({nowarn_function, yeccpars2_127_/1}).
-compile({nowarn_unused_function,  yeccpars2_127_/1}).
-file("src/compiler/parser/catena_parser.yrl", 644).
yeccpars2_127_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {pat_tuple, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-dialyzer({nowarn_function, yeccpars2_129_/1}).
-compile({nowarn_unused_function,  yeccpars2_129_/1}).
-file("src/compiler/parser/catena_parser.yrl", 638).
yeccpars2_129_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-dialyzer({nowarn_function, yeccpars2_130_/1}).
-compile({nowarn_unused_function,  yeccpars2_130_/1}).
-file("src/compiler/parser/catena_parser.yrl", 641).
yeccpars2_130_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-dialyzer({nowarn_function, yeccpars2_132_/1}).
-compile({nowarn_unused_function,  yeccpars2_132_/1}).
-file("src/compiler/parser/catena_parser.yrl", 664).
yeccpars2_132_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-dialyzer({nowarn_function, yeccpars2_134_/1}).
-compile({nowarn_unused_function,  yeccpars2_134_/1}).
-file("src/compiler/parser/catena_parser.yrl", 647).
yeccpars2_134_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_record, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-dialyzer({nowarn_function, yeccpars2_136_/1}).
-compile({nowarn_unused_function,  yeccpars2_136_/1}).
-file("src/compiler/parser/catena_parser.yrl", 669).
yeccpars2_136_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-dialyzer({nowarn_function, yeccpars2_138_/1}).
-compile({nowarn_unused_function,  yeccpars2_138_/1}).
-file("src/compiler/parser/catena_parser.yrl", 666).
yeccpars2_138_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                           
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-dialyzer({nowarn_function, yeccpars2_139_/1}).
-compile({nowarn_unused_function,  yeccpars2_139_/1}).
-file("src/compiler/parser/catena_parser.yrl", 650).
yeccpars2_139_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {pat_record, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-dialyzer({nowarn_function, yeccpars2_140_/1}).
-compile({nowarn_unused_function,  yeccpars2_140_/1}).
-file("src/compiler/parser/catena_parser.yrl", 518).
yeccpars2_140_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                              
    {transform_sig, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-dialyzer({nowarn_function, yeccpars2_142_/1}).
-compile({nowarn_unused_function,  yeccpars2_142_/1}).
-file("src/compiler/parser/catena_parser.yrl", 511).
yeccpars2_142_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    make_error_declaration(extract_location(___1), "Missing '=' or expression in transform declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-dialyzer({nowarn_function, yeccpars2_144_/1}).
-compile({nowarn_unused_function,  yeccpars2_144_/1}).
-file("src/compiler/parser/catena_parser.yrl", 789).
yeccpars2_144_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                ___1
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-dialyzer({nowarn_function, yeccpars2_145_/1}).
-compile({nowarn_unused_function,  yeccpars2_145_/1}).
-file("src/compiler/parser/catena_parser.yrl", 787).
yeccpars2_145_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-dialyzer({nowarn_function, yeccpars2_146_/1}).
-compile({nowarn_unused_function,  yeccpars2_146_/1}).
-file("src/compiler/parser/catena_parser.yrl", 755).
yeccpars2_146_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-dialyzer({nowarn_function, yeccpars2_148_/1}).
-compile({nowarn_unused_function,  yeccpars2_148_/1}).
-file("src/compiler/parser/catena_parser.yrl", 676).
yeccpars2_148_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                 
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-dialyzer({nowarn_function, yeccpars2_149_/1}).
-compile({nowarn_unused_function,  yeccpars2_149_/1}).
-file("src/compiler/parser/catena_parser.yrl", 752).
yeccpars2_149_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-dialyzer({nowarn_function, yeccpars2_150_/1}).
-compile({nowarn_unused_function,  yeccpars2_150_/1}).
-file("src/compiler/parser/catena_parser.yrl", 743).
yeccpars2_150_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-dialyzer({nowarn_function, yeccpars2_151_/1}).
-compile({nowarn_unused_function,  yeccpars2_151_/1}).
-file("src/compiler/parser/catena_parser.yrl", 681).
yeccpars2_151_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                ___1
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-dialyzer({nowarn_function, yeccpars2_152_/1}).
-compile({nowarn_unused_function,  yeccpars2_152_/1}).
-file("src/compiler/parser/catena_parser.yrl", 791).
yeccpars2_152_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-dialyzer({nowarn_function, yeccpars2_154_/1}).
-compile({nowarn_unused_function,  yeccpars2_154_/1}).
-file("src/compiler/parser/catena_parser.yrl", 842).
yeccpars2_154_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-dialyzer({nowarn_function, yeccpars2_157_/1}).
-compile({nowarn_unused_function,  yeccpars2_157_/1}).
-file("src/compiler/parser/catena_parser.yrl", 839).
yeccpars2_157_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-dialyzer({nowarn_function, yeccpars2_161_/1}).
-compile({nowarn_unused_function,  yeccpars2_161_/1}).
-file("src/compiler/parser/catena_parser.yrl", 757).
yeccpars2_161_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-dialyzer({nowarn_function, yeccpars2_165_/1}).
-compile({nowarn_unused_function,  yeccpars2_165_/1}).
-file("src/compiler/parser/catena_parser.yrl", 845).
yeccpars2_165_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-dialyzer({nowarn_function, yeccpars2_166_/1}).
-compile({nowarn_unused_function,  yeccpars2_166_/1}).
-file("src/compiler/parser/catena_parser.yrl", 760).
yeccpars2_166_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-dialyzer({nowarn_function, yeccpars2_170_/1}).
-compile({nowarn_unused_function,  yeccpars2_170_/1}).
-file("src/compiler/parser/catena_parser.yrl", 830).
yeccpars2_170_(__Stack0) ->
 [begin
                           
    []
  end | __Stack0].

-compile({inline,yeccpars2_172_/1}).
-dialyzer({nowarn_function, yeccpars2_172_/1}).
-compile({nowarn_unused_function,  yeccpars2_172_/1}).
-file("src/compiler/parser/catena_parser.yrl", 832).
yeccpars2_172_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    ___1
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-dialyzer({nowarn_function, yeccpars2_173_/1}).
-compile({nowarn_unused_function,  yeccpars2_173_/1}).
-file("src/compiler/parser/catena_parser.yrl", 824).
yeccpars2_173_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_192_/1}).
-dialyzer({nowarn_function, yeccpars2_192_/1}).
-compile({nowarn_unused_function,  yeccpars2_192_/1}).
-file("src/compiler/parser/catena_parser.yrl", 698).
yeccpars2_192_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, star, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_193_/1}).
-dialyzer({nowarn_function, yeccpars2_193_/1}).
-compile({nowarn_unused_function,  yeccpars2_193_/1}).
-file("src/compiler/parser/catena_parser.yrl", 701).
yeccpars2_193_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, slash, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_194_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_194_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_194_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
'yeccpars2_194_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_and/1}).
-dialyzer({nowarn_function, yeccpars2_194_and/1}).
-compile({nowarn_unused_function,  yeccpars2_194_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_194_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_194_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_comma/1}).
-dialyzer({nowarn_function, yeccpars2_194_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_194_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_cons/1}).
-dialyzer({nowarn_function, yeccpars2_194_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_194_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_do/1}).
-dialyzer({nowarn_function, yeccpars2_194_do/1}).
-compile({nowarn_unused_function,  yeccpars2_194_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_dot/1}).
-dialyzer({nowarn_function, yeccpars2_194_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_194_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_effect/1}).
-dialyzer({nowarn_function, yeccpars2_194_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_194_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_end/1}).
-dialyzer({nowarn_function, yeccpars2_194_end/1}).
-compile({nowarn_unused_function,  yeccpars2_194_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_equals/1}).
-dialyzer({nowarn_function, yeccpars2_194_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_194_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_error/1}).
-dialyzer({nowarn_function, yeccpars2_194_error/1}).
-compile({nowarn_unused_function,  yeccpars2_194_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_float/1}).
-dialyzer({nowarn_function, yeccpars2_194_float/1}).
-compile({nowarn_unused_function,  yeccpars2_194_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_fn/1}).
-dialyzer({nowarn_function, yeccpars2_194_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_194_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_handle/1}).
-dialyzer({nowarn_function, yeccpars2_194_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_194_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_in/1}).
-dialyzer({nowarn_function, yeccpars2_194_in/1}).
-compile({nowarn_unused_function,  yeccpars2_194_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_instance/1}).
-dialyzer({nowarn_function, yeccpars2_194_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_194_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_integer/1}).
-dialyzer({nowarn_function, yeccpars2_194_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_194_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_194_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_194_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_194_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_194_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_let/1}).
-dialyzer({nowarn_function, yeccpars2_194_let/1}).
-compile({nowarn_unused_function,  yeccpars2_194_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_194_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_194_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_194_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_194_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_match/1}).
-dialyzer({nowarn_function, yeccpars2_194_match/1}).
-compile({nowarn_unused_function,  yeccpars2_194_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_of/1}).
-dialyzer({nowarn_function, yeccpars2_194_of/1}).
-compile({nowarn_unused_function,  yeccpars2_194_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_or/1}).
-dialyzer({nowarn_function, yeccpars2_194_or/1}).
-compile({nowarn_unused_function,  yeccpars2_194_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_perform/1}).
-dialyzer({nowarn_function, yeccpars2_194_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_194_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_194_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_194_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_194_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_194_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_194_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_194_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_194_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_194_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_194_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_194_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_194_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_194_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_string/1}).
-dialyzer({nowarn_function, yeccpars2_194_string/1}).
-compile({nowarn_unused_function,  yeccpars2_194_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_then/1}).
-dialyzer({nowarn_function, yeccpars2_194_then/1}).
-compile({nowarn_unused_function,  yeccpars2_194_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_trait/1}).
-dialyzer({nowarn_function, yeccpars2_194_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_194_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_transform/1}).
-dialyzer({nowarn_function, yeccpars2_194_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_194_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_type/1}).
-dialyzer({nowarn_function, yeccpars2_194_type/1}).
-compile({nowarn_unused_function,  yeccpars2_194_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_194_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_194_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_194_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 713).
yeccpars2_194_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, setoid_neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_195_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_195_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_195_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
'yeccpars2_195_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_and/1}).
-dialyzer({nowarn_function, yeccpars2_195_and/1}).
-compile({nowarn_unused_function,  yeccpars2_195_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_195_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_195_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_comma/1}).
-dialyzer({nowarn_function, yeccpars2_195_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_195_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_cons/1}).
-dialyzer({nowarn_function, yeccpars2_195_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_195_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_do/1}).
-dialyzer({nowarn_function, yeccpars2_195_do/1}).
-compile({nowarn_unused_function,  yeccpars2_195_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_dot/1}).
-dialyzer({nowarn_function, yeccpars2_195_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_195_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_effect/1}).
-dialyzer({nowarn_function, yeccpars2_195_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_195_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_end/1}).
-dialyzer({nowarn_function, yeccpars2_195_end/1}).
-compile({nowarn_unused_function,  yeccpars2_195_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_equals/1}).
-dialyzer({nowarn_function, yeccpars2_195_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_195_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_error/1}).
-dialyzer({nowarn_function, yeccpars2_195_error/1}).
-compile({nowarn_unused_function,  yeccpars2_195_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_float/1}).
-dialyzer({nowarn_function, yeccpars2_195_float/1}).
-compile({nowarn_unused_function,  yeccpars2_195_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_fn/1}).
-dialyzer({nowarn_function, yeccpars2_195_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_195_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_handle/1}).
-dialyzer({nowarn_function, yeccpars2_195_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_195_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_in/1}).
-dialyzer({nowarn_function, yeccpars2_195_in/1}).
-compile({nowarn_unused_function,  yeccpars2_195_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_instance/1}).
-dialyzer({nowarn_function, yeccpars2_195_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_195_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_integer/1}).
-dialyzer({nowarn_function, yeccpars2_195_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_195_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_195_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_195_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_195_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_195_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_let/1}).
-dialyzer({nowarn_function, yeccpars2_195_let/1}).
-compile({nowarn_unused_function,  yeccpars2_195_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_195_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_195_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_195_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_195_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_match/1}).
-dialyzer({nowarn_function, yeccpars2_195_match/1}).
-compile({nowarn_unused_function,  yeccpars2_195_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_of/1}).
-dialyzer({nowarn_function, yeccpars2_195_of/1}).
-compile({nowarn_unused_function,  yeccpars2_195_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_or/1}).
-dialyzer({nowarn_function, yeccpars2_195_or/1}).
-compile({nowarn_unused_function,  yeccpars2_195_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_perform/1}).
-dialyzer({nowarn_function, yeccpars2_195_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_195_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_195_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_195_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_195_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_195_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_195_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_195_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_195_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_195_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_195_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_195_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_195_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_195_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_string/1}).
-dialyzer({nowarn_function, yeccpars2_195_string/1}).
-compile({nowarn_unused_function,  yeccpars2_195_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_then/1}).
-dialyzer({nowarn_function, yeccpars2_195_then/1}).
-compile({nowarn_unused_function,  yeccpars2_195_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_trait/1}).
-dialyzer({nowarn_function, yeccpars2_195_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_195_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_transform/1}).
-dialyzer({nowarn_function, yeccpars2_195_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_195_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_type/1}).
-dialyzer({nowarn_function, yeccpars2_195_type/1}).
-compile({nowarn_unused_function,  yeccpars2_195_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_195_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_195_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_195_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 710).
yeccpars2_195_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, setoid_eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_196_/1}).
-dialyzer({nowarn_function, yeccpars2_196_/1}).
-compile({nowarn_unused_function,  yeccpars2_196_/1}).
-file("src/compiler/parser/catena_parser.yrl", 736).
yeccpars2_196_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                             
    {binary_op, plus_plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_197_/1}).
-dialyzer({nowarn_function, yeccpars2_197_/1}).
-compile({nowarn_unused_function,  yeccpars2_197_/1}).
-file("src/compiler/parser/catena_parser.yrl", 692).
yeccpars2_197_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-dialyzer({nowarn_function, yeccpars2_198_/1}).
-compile({nowarn_unused_function,  yeccpars2_198_/1}).
-file("src/compiler/parser/catena_parser.yrl", 689).
yeccpars2_198_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, pipe_right, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-dialyzer({nowarn_function, yeccpars2_199_/1}).
-compile({nowarn_unused_function,  yeccpars2_199_/1}).
-file("src/compiler/parser/catena_parser.yrl", 732).
yeccpars2_199_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, 'or', ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_200_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_200_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_200_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
'yeccpars2_200_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_and/1}).
-dialyzer({nowarn_function, yeccpars2_200_and/1}).
-compile({nowarn_unused_function,  yeccpars2_200_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_200_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_200_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_comma/1}).
-dialyzer({nowarn_function, yeccpars2_200_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_200_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_cons/1}).
-dialyzer({nowarn_function, yeccpars2_200_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_200_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_do/1}).
-dialyzer({nowarn_function, yeccpars2_200_do/1}).
-compile({nowarn_unused_function,  yeccpars2_200_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_dot/1}).
-dialyzer({nowarn_function, yeccpars2_200_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_200_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_effect/1}).
-dialyzer({nowarn_function, yeccpars2_200_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_200_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_end/1}).
-dialyzer({nowarn_function, yeccpars2_200_end/1}).
-compile({nowarn_unused_function,  yeccpars2_200_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_equals/1}).
-dialyzer({nowarn_function, yeccpars2_200_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_200_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_error/1}).
-dialyzer({nowarn_function, yeccpars2_200_error/1}).
-compile({nowarn_unused_function,  yeccpars2_200_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_float/1}).
-dialyzer({nowarn_function, yeccpars2_200_float/1}).
-compile({nowarn_unused_function,  yeccpars2_200_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_fn/1}).
-dialyzer({nowarn_function, yeccpars2_200_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_200_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_handle/1}).
-dialyzer({nowarn_function, yeccpars2_200_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_200_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_in/1}).
-dialyzer({nowarn_function, yeccpars2_200_in/1}).
-compile({nowarn_unused_function,  yeccpars2_200_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_instance/1}).
-dialyzer({nowarn_function, yeccpars2_200_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_200_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_integer/1}).
-dialyzer({nowarn_function, yeccpars2_200_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_200_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_200_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_200_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_let/1}).
-dialyzer({nowarn_function, yeccpars2_200_let/1}).
-compile({nowarn_unused_function,  yeccpars2_200_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_200_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_200_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_200_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_match/1}).
-dialyzer({nowarn_function, yeccpars2_200_match/1}).
-compile({nowarn_unused_function,  yeccpars2_200_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_of/1}).
-dialyzer({nowarn_function, yeccpars2_200_of/1}).
-compile({nowarn_unused_function,  yeccpars2_200_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_or/1}).
-dialyzer({nowarn_function, yeccpars2_200_or/1}).
-compile({nowarn_unused_function,  yeccpars2_200_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_perform/1}).
-dialyzer({nowarn_function, yeccpars2_200_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_200_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_200_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_200_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_200_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_200_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_200_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_200_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_200_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_200_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_200_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_200_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_200_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_200_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_string/1}).
-dialyzer({nowarn_function, yeccpars2_200_string/1}).
-compile({nowarn_unused_function,  yeccpars2_200_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_then/1}).
-dialyzer({nowarn_function, yeccpars2_200_then/1}).
-compile({nowarn_unused_function,  yeccpars2_200_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_trait/1}).
-dialyzer({nowarn_function, yeccpars2_200_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_200_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_transform/1}).
-dialyzer({nowarn_function, yeccpars2_200_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_200_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_type/1}).
-dialyzer({nowarn_function, yeccpars2_200_type/1}).
-compile({nowarn_unused_function,  yeccpars2_200_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_200_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_200_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_200_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 707).
yeccpars2_200_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-dialyzer({nowarn_function, yeccpars2_201_/1}).
-compile({nowarn_unused_function,  yeccpars2_201_/1}).
-file("src/compiler/parser/catena_parser.yrl", 695).
yeccpars2_201_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, minus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_202_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_202_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_202_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
'yeccpars2_202_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_and/1}).
-dialyzer({nowarn_function, yeccpars2_202_and/1}).
-compile({nowarn_unused_function,  yeccpars2_202_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_202_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_202_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_comma/1}).
-dialyzer({nowarn_function, yeccpars2_202_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_202_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_cons/1}).
-dialyzer({nowarn_function, yeccpars2_202_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_202_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_do/1}).
-dialyzer({nowarn_function, yeccpars2_202_do/1}).
-compile({nowarn_unused_function,  yeccpars2_202_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_dot/1}).
-dialyzer({nowarn_function, yeccpars2_202_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_202_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_effect/1}).
-dialyzer({nowarn_function, yeccpars2_202_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_202_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_end/1}).
-dialyzer({nowarn_function, yeccpars2_202_end/1}).
-compile({nowarn_unused_function,  yeccpars2_202_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_eq/1}).
-dialyzer({nowarn_function, yeccpars2_202_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_202_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_equals/1}).
-dialyzer({nowarn_function, yeccpars2_202_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_202_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_error/1}).
-dialyzer({nowarn_function, yeccpars2_202_error/1}).
-compile({nowarn_unused_function,  yeccpars2_202_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_float/1}).
-dialyzer({nowarn_function, yeccpars2_202_float/1}).
-compile({nowarn_unused_function,  yeccpars2_202_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_fn/1}).
-dialyzer({nowarn_function, yeccpars2_202_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_202_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_handle/1}).
-dialyzer({nowarn_function, yeccpars2_202_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_202_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_in/1}).
-dialyzer({nowarn_function, yeccpars2_202_in/1}).
-compile({nowarn_unused_function,  yeccpars2_202_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_instance/1}).
-dialyzer({nowarn_function, yeccpars2_202_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_202_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_integer/1}).
-dialyzer({nowarn_function, yeccpars2_202_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_202_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_202_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_202_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_202_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_202_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_let/1}).
-dialyzer({nowarn_function, yeccpars2_202_let/1}).
-compile({nowarn_unused_function,  yeccpars2_202_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_202_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_202_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_202_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_202_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_match/1}).
-dialyzer({nowarn_function, yeccpars2_202_match/1}).
-compile({nowarn_unused_function,  yeccpars2_202_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_neq/1}).
-dialyzer({nowarn_function, yeccpars2_202_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_202_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_of/1}).
-dialyzer({nowarn_function, yeccpars2_202_of/1}).
-compile({nowarn_unused_function,  yeccpars2_202_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_or/1}).
-dialyzer({nowarn_function, yeccpars2_202_or/1}).
-compile({nowarn_unused_function,  yeccpars2_202_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_perform/1}).
-dialyzer({nowarn_function, yeccpars2_202_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_202_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_202_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_202_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_202_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_202_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_202_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_202_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_202_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_202_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_202_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_202_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_202_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_202_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_202_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_202_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_202_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_202_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_string/1}).
-dialyzer({nowarn_function, yeccpars2_202_string/1}).
-compile({nowarn_unused_function,  yeccpars2_202_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_then/1}).
-dialyzer({nowarn_function, yeccpars2_202_then/1}).
-compile({nowarn_unused_function,  yeccpars2_202_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_trait/1}).
-dialyzer({nowarn_function, yeccpars2_202_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_202_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_transform/1}).
-dialyzer({nowarn_function, yeccpars2_202_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_202_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_type/1}).
-dialyzer({nowarn_function, yeccpars2_202_type/1}).
-compile({nowarn_unused_function,  yeccpars2_202_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_202_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_202_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_202_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 722).
yeccpars2_202_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_203_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_203_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_203_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
'yeccpars2_203_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_and/1}).
-dialyzer({nowarn_function, yeccpars2_203_and/1}).
-compile({nowarn_unused_function,  yeccpars2_203_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_203_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_203_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_comma/1}).
-dialyzer({nowarn_function, yeccpars2_203_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_203_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_cons/1}).
-dialyzer({nowarn_function, yeccpars2_203_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_203_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_do/1}).
-dialyzer({nowarn_function, yeccpars2_203_do/1}).
-compile({nowarn_unused_function,  yeccpars2_203_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_dot/1}).
-dialyzer({nowarn_function, yeccpars2_203_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_203_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_effect/1}).
-dialyzer({nowarn_function, yeccpars2_203_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_203_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_end/1}).
-dialyzer({nowarn_function, yeccpars2_203_end/1}).
-compile({nowarn_unused_function,  yeccpars2_203_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_eq/1}).
-dialyzer({nowarn_function, yeccpars2_203_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_203_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_equals/1}).
-dialyzer({nowarn_function, yeccpars2_203_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_203_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_error/1}).
-dialyzer({nowarn_function, yeccpars2_203_error/1}).
-compile({nowarn_unused_function,  yeccpars2_203_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_float/1}).
-dialyzer({nowarn_function, yeccpars2_203_float/1}).
-compile({nowarn_unused_function,  yeccpars2_203_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_fn/1}).
-dialyzer({nowarn_function, yeccpars2_203_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_203_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_handle/1}).
-dialyzer({nowarn_function, yeccpars2_203_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_203_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_in/1}).
-dialyzer({nowarn_function, yeccpars2_203_in/1}).
-compile({nowarn_unused_function,  yeccpars2_203_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_instance/1}).
-dialyzer({nowarn_function, yeccpars2_203_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_203_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_integer/1}).
-dialyzer({nowarn_function, yeccpars2_203_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_203_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_203_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_203_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_203_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_203_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_let/1}).
-dialyzer({nowarn_function, yeccpars2_203_let/1}).
-compile({nowarn_unused_function,  yeccpars2_203_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_203_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_203_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_203_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_203_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_match/1}).
-dialyzer({nowarn_function, yeccpars2_203_match/1}).
-compile({nowarn_unused_function,  yeccpars2_203_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_neq/1}).
-dialyzer({nowarn_function, yeccpars2_203_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_203_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_of/1}).
-dialyzer({nowarn_function, yeccpars2_203_of/1}).
-compile({nowarn_unused_function,  yeccpars2_203_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_or/1}).
-dialyzer({nowarn_function, yeccpars2_203_or/1}).
-compile({nowarn_unused_function,  yeccpars2_203_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_perform/1}).
-dialyzer({nowarn_function, yeccpars2_203_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_203_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_203_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_203_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_203_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_203_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_203_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_203_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_203_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_203_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_203_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_203_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_203_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_203_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_203_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_203_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_203_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_203_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_string/1}).
-dialyzer({nowarn_function, yeccpars2_203_string/1}).
-compile({nowarn_unused_function,  yeccpars2_203_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_then/1}).
-dialyzer({nowarn_function, yeccpars2_203_then/1}).
-compile({nowarn_unused_function,  yeccpars2_203_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_trait/1}).
-dialyzer({nowarn_function, yeccpars2_203_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_203_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_transform/1}).
-dialyzer({nowarn_function, yeccpars2_203_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_203_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_type/1}).
-dialyzer({nowarn_function, yeccpars2_203_type/1}).
-compile({nowarn_unused_function,  yeccpars2_203_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_203_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_203_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_203_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 716).
yeccpars2_203_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_204_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_204_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_204_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
'yeccpars2_204_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_and/1}).
-dialyzer({nowarn_function, yeccpars2_204_and/1}).
-compile({nowarn_unused_function,  yeccpars2_204_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_204_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_204_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_comma/1}).
-dialyzer({nowarn_function, yeccpars2_204_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_204_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_cons/1}).
-dialyzer({nowarn_function, yeccpars2_204_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_204_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_do/1}).
-dialyzer({nowarn_function, yeccpars2_204_do/1}).
-compile({nowarn_unused_function,  yeccpars2_204_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_dot/1}).
-dialyzer({nowarn_function, yeccpars2_204_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_204_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_effect/1}).
-dialyzer({nowarn_function, yeccpars2_204_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_204_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_end/1}).
-dialyzer({nowarn_function, yeccpars2_204_end/1}).
-compile({nowarn_unused_function,  yeccpars2_204_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_eq/1}).
-dialyzer({nowarn_function, yeccpars2_204_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_204_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_equals/1}).
-dialyzer({nowarn_function, yeccpars2_204_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_204_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_error/1}).
-dialyzer({nowarn_function, yeccpars2_204_error/1}).
-compile({nowarn_unused_function,  yeccpars2_204_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_float/1}).
-dialyzer({nowarn_function, yeccpars2_204_float/1}).
-compile({nowarn_unused_function,  yeccpars2_204_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_fn/1}).
-dialyzer({nowarn_function, yeccpars2_204_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_204_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_handle/1}).
-dialyzer({nowarn_function, yeccpars2_204_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_204_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_in/1}).
-dialyzer({nowarn_function, yeccpars2_204_in/1}).
-compile({nowarn_unused_function,  yeccpars2_204_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_instance/1}).
-dialyzer({nowarn_function, yeccpars2_204_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_204_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_integer/1}).
-dialyzer({nowarn_function, yeccpars2_204_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_204_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_204_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_204_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_204_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_204_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_let/1}).
-dialyzer({nowarn_function, yeccpars2_204_let/1}).
-compile({nowarn_unused_function,  yeccpars2_204_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_204_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_204_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_204_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_204_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_match/1}).
-dialyzer({nowarn_function, yeccpars2_204_match/1}).
-compile({nowarn_unused_function,  yeccpars2_204_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_neq/1}).
-dialyzer({nowarn_function, yeccpars2_204_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_204_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_of/1}).
-dialyzer({nowarn_function, yeccpars2_204_of/1}).
-compile({nowarn_unused_function,  yeccpars2_204_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_or/1}).
-dialyzer({nowarn_function, yeccpars2_204_or/1}).
-compile({nowarn_unused_function,  yeccpars2_204_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_perform/1}).
-dialyzer({nowarn_function, yeccpars2_204_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_204_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_204_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_204_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_204_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_204_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_204_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_204_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_204_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_204_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_204_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_204_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_204_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_204_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_204_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_204_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_204_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_204_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_string/1}).
-dialyzer({nowarn_function, yeccpars2_204_string/1}).
-compile({nowarn_unused_function,  yeccpars2_204_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_then/1}).
-dialyzer({nowarn_function, yeccpars2_204_then/1}).
-compile({nowarn_unused_function,  yeccpars2_204_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_trait/1}).
-dialyzer({nowarn_function, yeccpars2_204_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_204_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_transform/1}).
-dialyzer({nowarn_function, yeccpars2_204_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_204_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_type/1}).
-dialyzer({nowarn_function, yeccpars2_204_type/1}).
-compile({nowarn_unused_function,  yeccpars2_204_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_204_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_204_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_204_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 725).
yeccpars2_204_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_205_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_205_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_205_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
'yeccpars2_205_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_and/1}).
-dialyzer({nowarn_function, yeccpars2_205_and/1}).
-compile({nowarn_unused_function,  yeccpars2_205_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_205_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_205_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_comma/1}).
-dialyzer({nowarn_function, yeccpars2_205_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_205_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_cons/1}).
-dialyzer({nowarn_function, yeccpars2_205_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_205_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_do/1}).
-dialyzer({nowarn_function, yeccpars2_205_do/1}).
-compile({nowarn_unused_function,  yeccpars2_205_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_dot/1}).
-dialyzer({nowarn_function, yeccpars2_205_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_205_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_effect/1}).
-dialyzer({nowarn_function, yeccpars2_205_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_205_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_end/1}).
-dialyzer({nowarn_function, yeccpars2_205_end/1}).
-compile({nowarn_unused_function,  yeccpars2_205_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_eq/1}).
-dialyzer({nowarn_function, yeccpars2_205_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_205_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_equals/1}).
-dialyzer({nowarn_function, yeccpars2_205_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_205_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_error/1}).
-dialyzer({nowarn_function, yeccpars2_205_error/1}).
-compile({nowarn_unused_function,  yeccpars2_205_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_float/1}).
-dialyzer({nowarn_function, yeccpars2_205_float/1}).
-compile({nowarn_unused_function,  yeccpars2_205_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_fn/1}).
-dialyzer({nowarn_function, yeccpars2_205_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_205_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_handle/1}).
-dialyzer({nowarn_function, yeccpars2_205_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_205_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_in/1}).
-dialyzer({nowarn_function, yeccpars2_205_in/1}).
-compile({nowarn_unused_function,  yeccpars2_205_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_instance/1}).
-dialyzer({nowarn_function, yeccpars2_205_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_205_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_integer/1}).
-dialyzer({nowarn_function, yeccpars2_205_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_205_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_205_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_205_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_205_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_205_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_let/1}).
-dialyzer({nowarn_function, yeccpars2_205_let/1}).
-compile({nowarn_unused_function,  yeccpars2_205_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_205_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_205_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_205_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_205_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_match/1}).
-dialyzer({nowarn_function, yeccpars2_205_match/1}).
-compile({nowarn_unused_function,  yeccpars2_205_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_neq/1}).
-dialyzer({nowarn_function, yeccpars2_205_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_205_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_of/1}).
-dialyzer({nowarn_function, yeccpars2_205_of/1}).
-compile({nowarn_unused_function,  yeccpars2_205_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_or/1}).
-dialyzer({nowarn_function, yeccpars2_205_or/1}).
-compile({nowarn_unused_function,  yeccpars2_205_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_perform/1}).
-dialyzer({nowarn_function, yeccpars2_205_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_205_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_205_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_205_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_205_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_205_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_205_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_205_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_205_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_205_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_205_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_205_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_205_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_205_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_setoid_eq/1}).
-dialyzer({nowarn_function, yeccpars2_205_setoid_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_205_setoid_eq/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_setoid_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_setoid_neq/1}).
-dialyzer({nowarn_function, yeccpars2_205_setoid_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_205_setoid_neq/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_setoid_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_string/1}).
-dialyzer({nowarn_function, yeccpars2_205_string/1}).
-compile({nowarn_unused_function,  yeccpars2_205_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_then/1}).
-dialyzer({nowarn_function, yeccpars2_205_then/1}).
-compile({nowarn_unused_function,  yeccpars2_205_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_trait/1}).
-dialyzer({nowarn_function, yeccpars2_205_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_205_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_transform/1}).
-dialyzer({nowarn_function, yeccpars2_205_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_205_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_type/1}).
-dialyzer({nowarn_function, yeccpars2_205_type/1}).
-compile({nowarn_unused_function,  yeccpars2_205_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_205_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_205_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_205_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 719).
yeccpars2_205_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_206_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_206_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_206_$end'/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
'yeccpars2_206_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_and/1}).
-dialyzer({nowarn_function, yeccpars2_206_and/1}).
-compile({nowarn_unused_function,  yeccpars2_206_and/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_and(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_206_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_206_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_comma/1}).
-dialyzer({nowarn_function, yeccpars2_206_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_206_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_cons/1}).
-dialyzer({nowarn_function, yeccpars2_206_cons/1}).
-compile({nowarn_unused_function,  yeccpars2_206_cons/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_cons(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_do/1}).
-dialyzer({nowarn_function, yeccpars2_206_do/1}).
-compile({nowarn_unused_function,  yeccpars2_206_do/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_do(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_dot/1}).
-dialyzer({nowarn_function, yeccpars2_206_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_206_dot/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_effect/1}).
-dialyzer({nowarn_function, yeccpars2_206_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_206_effect/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_end/1}).
-dialyzer({nowarn_function, yeccpars2_206_end/1}).
-compile({nowarn_unused_function,  yeccpars2_206_end/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_equals/1}).
-dialyzer({nowarn_function, yeccpars2_206_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_206_equals/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_error/1}).
-dialyzer({nowarn_function, yeccpars2_206_error/1}).
-compile({nowarn_unused_function,  yeccpars2_206_error/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_float/1}).
-dialyzer({nowarn_function, yeccpars2_206_float/1}).
-compile({nowarn_unused_function,  yeccpars2_206_float/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_fn/1}).
-dialyzer({nowarn_function, yeccpars2_206_fn/1}).
-compile({nowarn_unused_function,  yeccpars2_206_fn/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_fn(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_handle/1}).
-dialyzer({nowarn_function, yeccpars2_206_handle/1}).
-compile({nowarn_unused_function,  yeccpars2_206_handle/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_handle(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_in/1}).
-dialyzer({nowarn_function, yeccpars2_206_in/1}).
-compile({nowarn_unused_function,  yeccpars2_206_in/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_instance/1}).
-dialyzer({nowarn_function, yeccpars2_206_instance/1}).
-compile({nowarn_unused_function,  yeccpars2_206_instance/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_instance(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_integer/1}).
-dialyzer({nowarn_function, yeccpars2_206_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_206_integer/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_206_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_206_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_let/1}).
-dialyzer({nowarn_function, yeccpars2_206_let/1}).
-compile({nowarn_unused_function,  yeccpars2_206_let/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_206_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lower_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_206_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_206_lparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_match/1}).
-dialyzer({nowarn_function, yeccpars2_206_match/1}).
-compile({nowarn_unused_function,  yeccpars2_206_match/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_match(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_of/1}).
-dialyzer({nowarn_function, yeccpars2_206_of/1}).
-compile({nowarn_unused_function,  yeccpars2_206_of/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_of(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_or/1}).
-dialyzer({nowarn_function, yeccpars2_206_or/1}).
-compile({nowarn_unused_function,  yeccpars2_206_or/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_or(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_perform/1}).
-dialyzer({nowarn_function, yeccpars2_206_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_206_perform/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_206_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_206_pipe/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_206_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_206_pipe_right/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_206_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_206_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_206_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_206_rbracket/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_206_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_206_rparen/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_semicolon/1}).
-dialyzer({nowarn_function, yeccpars2_206_semicolon/1}).
-compile({nowarn_unused_function,  yeccpars2_206_semicolon/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_semicolon(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_string/1}).
-dialyzer({nowarn_function, yeccpars2_206_string/1}).
-compile({nowarn_unused_function,  yeccpars2_206_string/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_then/1}).
-dialyzer({nowarn_function, yeccpars2_206_then/1}).
-compile({nowarn_unused_function,  yeccpars2_206_then/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_trait/1}).
-dialyzer({nowarn_function, yeccpars2_206_trait/1}).
-compile({nowarn_unused_function,  yeccpars2_206_trait/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_trait(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_transform/1}).
-dialyzer({nowarn_function, yeccpars2_206_transform/1}).
-compile({nowarn_unused_function,  yeccpars2_206_transform/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_transform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_type/1}).
-dialyzer({nowarn_function, yeccpars2_206_type/1}).
-compile({nowarn_unused_function,  yeccpars2_206_type/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_type(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_206_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_206_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_206_upper_ident/1}).
-file("src/compiler/parser/catena_parser.yrl", 704).
yeccpars2_206_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_207_/1}).
-dialyzer({nowarn_function, yeccpars2_207_/1}).
-compile({nowarn_unused_function,  yeccpars2_207_/1}).
-file("src/compiler/parser/catena_parser.yrl", 740).
yeccpars2_207_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {cons_expr, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-dialyzer({nowarn_function, yeccpars2_208_/1}).
-compile({nowarn_unused_function,  yeccpars2_208_/1}).
-file("src/compiler/parser/catena_parser.yrl", 826).
yeccpars2_208_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-dialyzer({nowarn_function, yeccpars2_209_/1}).
-compile({nowarn_unused_function,  yeccpars2_209_/1}).
-file("src/compiler/parser/catena_parser.yrl", 729).
yeccpars2_209_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, 'and', ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-dialyzer({nowarn_function, yeccpars2_210_/1}).
-compile({nowarn_unused_function,  yeccpars2_210_/1}).
-file("src/compiler/parser/catena_parser.yrl", 885).
yeccpars2_210_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                 
    {perform_expr,
        extract_atom(___2),
        extract_atom(___4),
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-dialyzer({nowarn_function, yeccpars2_212_/1}).
-compile({nowarn_unused_function,  yeccpars2_212_/1}).
-file("src/compiler/parser/catena_parser.yrl", 544).
yeccpars2_212_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_220_/1}).
-dialyzer({nowarn_function, yeccpars2_220_/1}).
-compile({nowarn_unused_function,  yeccpars2_220_/1}).
-file("src/compiler/parser/catena_parser.yrl", 556).
yeccpars2_220_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {match_clause,
        ___2,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-dialyzer({nowarn_function, yeccpars2_221_/1}).
-compile({nowarn_unused_function,  yeccpars2_221_/1}).
-file("src/compiler/parser/catena_parser.yrl", 549).
yeccpars2_221_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    {match_clause,
        ___2,
        undefined,
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-dialyzer({nowarn_function, yeccpars2_224_/1}).
-compile({nowarn_unused_function,  yeccpars2_224_/1}).
-file("src/compiler/parser/catena_parser.yrl", 799).
yeccpars2_224_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     
    {match_expr, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-dialyzer({nowarn_function, yeccpars2_225_/1}).
-compile({nowarn_unused_function,  yeccpars2_225_/1}).
-file("src/compiler/parser/catena_parser.yrl", 546).
yeccpars2_225_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_226_/1}).
-dialyzer({nowarn_function, yeccpars2_226_/1}).
-compile({nowarn_unused_function,  yeccpars2_226_/1}).
-file("src/compiler/parser/catena_parser.yrl", 795).
yeccpars2_226_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {match_expr, undefined, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_230_/1}).
-dialyzer({nowarn_function, yeccpars2_230_/1}).
-compile({nowarn_unused_function,  yeccpars2_230_/1}).
-file("src/compiler/parser/catena_parser.yrl", 763).
yeccpars2_230_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    ___2
  end | __Stack].

-compile({inline,yeccpars2_231_/1}).
-dialyzer({nowarn_function, yeccpars2_231_/1}).
-compile({nowarn_unused_function,  yeccpars2_231_/1}).
-file("src/compiler/parser/catena_parser.yrl", 772).
yeccpars2_231_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_232_/1}).
-dialyzer({nowarn_function, yeccpars2_232_/1}).
-compile({nowarn_unused_function,  yeccpars2_232_/1}).
-file("src/compiler/parser/catena_parser.yrl", 770).
yeccpars2_232_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_233_/1}).
-dialyzer({nowarn_function, yeccpars2_233_/1}).
-compile({nowarn_unused_function,  yeccpars2_233_/1}).
-file("src/compiler/parser/catena_parser.yrl", 766).
yeccpars2_233_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {tuple_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_238_/1}).
-dialyzer({nowarn_function, yeccpars2_238_/1}).
-compile({nowarn_unused_function,  yeccpars2_238_/1}).
-file("src/compiler/parser/catena_parser.yrl", 775).
yeccpars2_238_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {let_expr,
        [{pat_var, extract_atom(___2), extract_location(___2)}, ___4],
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-dialyzer({nowarn_function, yeccpars2_240_/1}).
-compile({nowarn_unused_function,  yeccpars2_240_/1}).
-file("src/compiler/parser/catena_parser.yrl", 802).
yeccpars2_240_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {list_expr, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-dialyzer({nowarn_function, yeccpars2_241_/1}).
-compile({nowarn_unused_function,  yeccpars2_241_/1}).
-file("src/compiler/parser/catena_parser.yrl", 805).
yeccpars2_241_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {list_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-dialyzer({nowarn_function, yeccpars2_243_/1}).
-compile({nowarn_unused_function,  yeccpars2_243_/1}).
-file("src/compiler/parser/catena_parser.yrl", 815).
yeccpars2_243_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_245_/1}).
-dialyzer({nowarn_function, yeccpars2_245_/1}).
-compile({nowarn_unused_function,  yeccpars2_245_/1}).
-file("src/compiler/parser/catena_parser.yrl", 808).
yeccpars2_245_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    {record_expr, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-dialyzer({nowarn_function, yeccpars2_247_/1}).
-compile({nowarn_unused_function,  yeccpars2_247_/1}).
-file("src/compiler/parser/catena_parser.yrl", 820).
yeccpars2_247_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_249_/1}).
-dialyzer({nowarn_function, yeccpars2_249_/1}).
-compile({nowarn_unused_function,  yeccpars2_249_/1}).
-file("src/compiler/parser/catena_parser.yrl", 817).
yeccpars2_249_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_250_/1}).
-dialyzer({nowarn_function, yeccpars2_250_/1}).
-compile({nowarn_unused_function,  yeccpars2_250_/1}).
-file("src/compiler/parser/catena_parser.yrl", 811).
yeccpars2_250_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {record_expr, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_255_/1}).
-dialyzer({nowarn_function, yeccpars2_255_/1}).
-compile({nowarn_unused_function,  yeccpars2_255_/1}).
-file("src/compiler/parser/catena_parser.yrl", 901).
yeccpars2_255_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_259_/1}).
-dialyzer({nowarn_function, yeccpars2_259_/1}).
-compile({nowarn_unused_function,  yeccpars2_259_/1}).
-file("src/compiler/parser/catena_parser.yrl", 916).
yeccpars2_259_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_262_/1}).
-dialyzer({nowarn_function, yeccpars2_262_/1}).
-compile({nowarn_unused_function,  yeccpars2_262_/1}).
-file("src/compiler/parser/catena_parser.yrl", 578).
yeccpars2_262_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_264_/1}).
-dialyzer({nowarn_function, yeccpars2_264_/1}).
-compile({nowarn_unused_function,  yeccpars2_264_/1}).
-file("src/compiler/parser/catena_parser.yrl", 580).
yeccpars2_264_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_265_/1}).
-dialyzer({nowarn_function, yeccpars2_265_/1}).
-compile({nowarn_unused_function,  yeccpars2_265_/1}).
-file("src/compiler/parser/catena_parser.yrl", 578).
yeccpars2_265_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_266_/1}).
-dialyzer({nowarn_function, yeccpars2_266_/1}).
-compile({nowarn_unused_function,  yeccpars2_266_/1}).
-file("src/compiler/parser/catena_parser.yrl", 582).
yeccpars2_266_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_269_/1}).
-dialyzer({nowarn_function, yeccpars2_269_/1}).
-compile({nowarn_unused_function,  yeccpars2_269_/1}).
-file("src/compiler/parser/catena_parser.yrl", 919).
yeccpars2_269_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                   
    {operation_case,
        extract_atom(___1),
        ___3,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_270_/1}).
-dialyzer({nowarn_function, yeccpars2_270_/1}).
-compile({nowarn_unused_function,  yeccpars2_270_/1}).
-file("src/compiler/parser/catena_parser.yrl", 926).
yeccpars2_270_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {operation_case,
        extract_atom(___1),
        [],
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_271_/1}).
-dialyzer({nowarn_function, yeccpars2_271_/1}).
-compile({nowarn_unused_function,  yeccpars2_271_/1}).
-file("src/compiler/parser/catena_parser.yrl", 914).
yeccpars2_271_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_272_/1}).
-dialyzer({nowarn_function, yeccpars2_272_/1}).
-compile({nowarn_unused_function,  yeccpars2_272_/1}).
-file("src/compiler/parser/catena_parser.yrl", 906).
yeccpars2_272_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             
    {handler_clause,
        extract_atom(___1),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_273_/1}).
-dialyzer({nowarn_function, yeccpars2_273_/1}).
-compile({nowarn_unused_function,  yeccpars2_273_/1}).
-file("src/compiler/parser/catena_parser.yrl", 903).
yeccpars2_273_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_274_/1}).
-dialyzer({nowarn_function, yeccpars2_274_/1}).
-compile({nowarn_unused_function,  yeccpars2_274_/1}).
-file("src/compiler/parser/catena_parser.yrl", 894).
yeccpars2_274_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    {handle_expr,
        ___2,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_277_/1}).
-dialyzer({nowarn_function, yeccpars2_277_/1}).
-compile({nowarn_unused_function,  yeccpars2_277_/1}).
-file("src/compiler/parser/catena_parser.yrl", 782).
yeccpars2_277_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {lambda, [{pat_var, extract_atom(___2), extract_location(___2)}], ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_279_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_279_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_279_rbrace/1}).
-file("src/compiler/parser/catena_parser.yrl", 857).
yeccpars2_279_rbrace(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    [{do_return, ___1, extract_location(___1)}]
  end | __Stack].

-compile({inline,yeccpars2_279_/1}).
-dialyzer({nowarn_function, yeccpars2_279_/1}).
-compile({nowarn_unused_function,  yeccpars2_279_/1}).
-file("src/compiler/parser/catena_parser.yrl", 876).
yeccpars2_279_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      
    {do_action, ___1, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_283_/1}).
-dialyzer({nowarn_function, yeccpars2_283_/1}).
-compile({nowarn_unused_function,  yeccpars2_283_/1}).
-file("src/compiler/parser/catena_parser.yrl", 757).
yeccpars2_283_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_285_/1}).
-dialyzer({nowarn_function, yeccpars2_285_/1}).
-compile({nowarn_unused_function,  yeccpars2_285_/1}).
-file("src/compiler/parser/catena_parser.yrl", 868).
yeccpars2_285_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {do_bind, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_288_/1}).
-dialyzer({nowarn_function, yeccpars2_288_/1}).
-compile({nowarn_unused_function,  yeccpars2_288_/1}).
-file("src/compiler/parser/catena_parser.yrl", 872).
yeccpars2_288_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {do_let, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_290_/1}).
-dialyzer({nowarn_function, yeccpars2_290_/1}).
-compile({nowarn_unused_function,  yeccpars2_290_/1}).
-file("src/compiler/parser/catena_parser.yrl", 859).
yeccpars2_290_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_291_/1}).
-dialyzer({nowarn_function, yeccpars2_291_/1}).
-compile({nowarn_unused_function,  yeccpars2_291_/1}).
-file("src/compiler/parser/catena_parser.yrl", 853).
yeccpars2_291_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {do_expr, ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_292_/1}).
-dialyzer({nowarn_function, yeccpars2_292_/1}).
-compile({nowarn_unused_function,  yeccpars2_292_/1}).
-file("src/compiler/parser/catena_parser.yrl", 746).
yeccpars2_292_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {app, ___1, [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_294_/1}).
-dialyzer({nowarn_function, yeccpars2_294_/1}).
-compile({nowarn_unused_function,  yeccpars2_294_/1}).
-file("src/compiler/parser/catena_parser.yrl", 749).
yeccpars2_294_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {record_access, ___1, extract_atom(___3), extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_296_/1}).
-dialyzer({nowarn_function, yeccpars2_296_/1}).
-compile({nowarn_unused_function,  yeccpars2_296_/1}).
-file("src/compiler/parser/catena_parser.yrl", 678).
yeccpars2_296_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_298_/1}).
-dialyzer({nowarn_function, yeccpars2_298_/1}).
-compile({nowarn_unused_function,  yeccpars2_298_/1}).
-file("src/compiler/parser/catena_parser.yrl", 501).
yeccpars2_298_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                
    {transform_decl,
        extract_atom(___2),
        undefined,
        [{transform_clause, ___3, ___5, ___7, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_299_/1}).
-dialyzer({nowarn_function, yeccpars2_299_/1}).
-compile({nowarn_unused_function,  yeccpars2_299_/1}).
-file("src/compiler/parser/catena_parser.yrl", 494).
yeccpars2_299_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    {transform_decl,
        extract_atom(___2),
        undefined,
        [{transform_clause, ___3, undefined, ___5, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_301_/1}).
-dialyzer({nowarn_function, yeccpars2_301_/1}).
-compile({nowarn_unused_function,  yeccpars2_301_/1}).
-file("src/compiler/parser/catena_parser.yrl", 509).
yeccpars2_301_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    make_error_declaration(extract_location(___1), "Invalid transform name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_302_/1}).
-dialyzer({nowarn_function, yeccpars2_302_/1}).
-compile({nowarn_unused_function,  yeccpars2_302_/1}).
-file("src/compiler/parser/catena_parser.yrl", 361).
yeccpars2_302_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___1), "Incomplete trait declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_303_/1}).
-dialyzer({nowarn_function, yeccpars2_303_/1}).
-compile({nowarn_unused_function,  yeccpars2_303_/1}).
-file("src/compiler/parser/catena_parser.yrl", 283).
yeccpars2_303_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_304_/1}).
-dialyzer({nowarn_function, yeccpars2_304_/1}).
-compile({nowarn_unused_function,  yeccpars2_304_/1}).
-file("src/compiler/parser/catena_parser.yrl", 366).
yeccpars2_304_(__Stack0) ->
 [begin
                                  undefined
  end | __Stack0].

-compile({inline,yeccpars2_307_/1}).
-dialyzer({nowarn_function, yeccpars2_307_/1}).
-compile({nowarn_unused_function,  yeccpars2_307_/1}).
-file("src/compiler/parser/catena_parser.yrl", 365).
yeccpars2_307_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   ___2
  end | __Stack].

-compile({inline,yeccpars2_308_/1}).
-dialyzer({nowarn_function, yeccpars2_308_/1}).
-compile({nowarn_unused_function,  yeccpars2_308_/1}).
-file("src/compiler/parser/catena_parser.yrl", 369).
yeccpars2_308_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_310_/1}).
-dialyzer({nowarn_function, yeccpars2_310_/1}).
-compile({nowarn_unused_function,  yeccpars2_310_/1}).
-file("src/compiler/parser/catena_parser.yrl", 371).
yeccpars2_310_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_313_/1}).
-dialyzer({nowarn_function, yeccpars2_313_/1}).
-compile({nowarn_unused_function,  yeccpars2_313_/1}).
-file("src/compiler/parser/catena_parser.yrl", 381).
yeccpars2_313_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_317_/1}).
-dialyzer({nowarn_function, yeccpars2_317_/1}).
-compile({nowarn_unused_function,  yeccpars2_317_/1}).
-file("src/compiler/parser/catena_parser.yrl", 391).
yeccpars2_317_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {trait_sig, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_318_/1}).
-dialyzer({nowarn_function, yeccpars2_318_/1}).
-compile({nowarn_unused_function,  yeccpars2_318_/1}).
-file("src/compiler/parser/catena_parser.yrl", 399).
yeccpars2_318_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    make_error_declaration(extract_location(___2),
        "Invalid method signature. " ++
        "Common issues and solutions:\n" ++
        "  • Cannot use simple tuples as parameters: '(a, b) -> ...'\n" ++
        "  • Try: 'Pair a b -> ...' or '((a -> b), c) -> ...'\n" ++
        "  • See trait signatures documentation for examples", ___3)
  end | __Stack].

-compile({inline,yeccpars2_320_/1}).
-dialyzer({nowarn_function, yeccpars2_320_/1}).
-compile({nowarn_unused_function,  yeccpars2_320_/1}).
-file("src/compiler/parser/catena_parser.yrl", 395).
yeccpars2_320_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               
    {trait_default, extract_atom(___1), ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_321_/1}).
-dialyzer({nowarn_function, yeccpars2_321_/1}).
-compile({nowarn_unused_function,  yeccpars2_321_/1}).
-file("src/compiler/parser/catena_parser.yrl", 385).
yeccpars2_321_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                     
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_322_/1}).
-dialyzer({nowarn_function, yeccpars2_322_/1}).
-compile({nowarn_unused_function,  yeccpars2_322_/1}).
-file("src/compiler/parser/catena_parser.yrl", 383).
yeccpars2_322_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_323_/1}).
-dialyzer({nowarn_function, yeccpars2_323_/1}).
-compile({nowarn_unused_function,  yeccpars2_323_/1}).
-file("src/compiler/parser/catena_parser.yrl", 352).
yeccpars2_323_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                           
    {trait_decl,
        extract_atom(___2),
        ___3,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_324_/1}).
-dialyzer({nowarn_function, yeccpars2_324_/1}).
-compile({nowarn_unused_function,  yeccpars2_324_/1}).
-file("src/compiler/parser/catena_parser.yrl", 211).
yeccpars2_324_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {module_decl, extract_atom(___2), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_326_/1}).
-dialyzer({nowarn_function, yeccpars2_326_/1}).
-compile({nowarn_unused_function,  yeccpars2_326_/1}).
-file("src/compiler/parser/catena_parser.yrl", 215).
yeccpars2_326_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {module_decl,
        list_to_atom(atom_to_list(extract_atom(___2)) ++ "." ++ atom_to_list(extract_atom(___4))),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_327_/1}).
-dialyzer({nowarn_function, yeccpars2_327_/1}).
-compile({nowarn_unused_function,  yeccpars2_327_/1}).
-file("src/compiler/parser/catena_parser.yrl", 456).
yeccpars2_327_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_329_/1}).
-dialyzer({nowarn_function, yeccpars2_329_/1}).
-compile({nowarn_unused_function,  yeccpars2_329_/1}).
-file("src/compiler/parser/catena_parser.yrl", 448).
yeccpars2_329_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                 
    make_error_declaration(extract_location(___1), "Incomplete instance declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_330_/1}).
-dialyzer({nowarn_function, yeccpars2_330_/1}).
-compile({nowarn_unused_function,  yeccpars2_330_/1}).
-file("src/compiler/parser/catena_parser.yrl", 993).
yeccpars2_330_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_331_comma/1}).
-dialyzer({nowarn_function, yeccpars2_331_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_331_comma/1}).
-file("src/compiler/parser/catena_parser.yrl", 984).
yeccpars2_331_comma(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_331_double_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_331_double_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_331_double_arrow/1}).
-file("src/compiler/parser/catena_parser.yrl", 984).
yeccpars2_331_double_arrow(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_331_/1}).
-dialyzer({nowarn_function, yeccpars2_331_/1}).
-compile({nowarn_unused_function,  yeccpars2_331_/1}).
-file("src/compiler/parser/catena_parser.yrl", 452).
yeccpars2_331_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          [___1]
  end | __Stack].

-compile({inline,yeccpars2_335_/1}).
-dialyzer({nowarn_function, yeccpars2_335_/1}).
-compile({nowarn_unused_function,  yeccpars2_335_/1}).
-file("src/compiler/parser/catena_parser.yrl", 463).
yeccpars2_335_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_336_/1}).
-dialyzer({nowarn_function, yeccpars2_336_/1}).
-compile({nowarn_unused_function,  yeccpars2_336_/1}).
-file("src/compiler/parser/catena_parser.yrl", 430).
yeccpars2_336_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                      
    {instance_decl,
        extract_atom(___2),
        ___3,
        undefined,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_338_/1}).
-dialyzer({nowarn_function, yeccpars2_338_/1}).
-compile({nowarn_unused_function,  yeccpars2_338_/1}).
-file("src/compiler/parser/catena_parser.yrl", 567).
yeccpars2_338_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_341_/1}).
-dialyzer({nowarn_function, yeccpars2_341_/1}).
-compile({nowarn_unused_function,  yeccpars2_341_/1}).
-file("src/compiler/parser/catena_parser.yrl", 471).
yeccpars2_341_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   
    {extract_atom(___2), {lambda, ___3, ___5, extract_location(___1)}}
  end | __Stack].

-compile({inline,yeccpars2_342_/1}).
-dialyzer({nowarn_function, yeccpars2_342_/1}).
-compile({nowarn_unused_function,  yeccpars2_342_/1}).
-file("src/compiler/parser/catena_parser.yrl", 467).
yeccpars2_342_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_343_/1}).
-dialyzer({nowarn_function, yeccpars2_343_/1}).
-compile({nowarn_unused_function,  yeccpars2_343_/1}).
-file("src/compiler/parser/catena_parser.yrl", 465).
yeccpars2_343_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_344_/1}).
-dialyzer({nowarn_function, yeccpars2_344_/1}).
-compile({nowarn_unused_function,  yeccpars2_344_/1}).
-file("src/compiler/parser/catena_parser.yrl", 421).
yeccpars2_344_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                       
    {instance_decl,
        extract_atom(___2),
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_345_/1}).
-dialyzer({nowarn_function, yeccpars2_345_/1}).
-compile({nowarn_unused_function,  yeccpars2_345_/1}).
-file("src/compiler/parser/catena_parser.yrl", 453).
yeccpars2_345_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                             [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_348_/1}).
-dialyzer({nowarn_function, yeccpars2_348_/1}).
-compile({nowarn_unused_function,  yeccpars2_348_/1}).
-file("src/compiler/parser/catena_parser.yrl", 452).
yeccpars2_348_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                          [___1]
  end | __Stack].

-compile({inline,yeccpars2_352_/1}).
-dialyzer({nowarn_function, yeccpars2_352_/1}).
-compile({nowarn_unused_function,  yeccpars2_352_/1}).
-file("src/compiler/parser/catena_parser.yrl", 439).
yeccpars2_352_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                        
    {instance_decl,
        extract_atom(___4),
        ___5,
        ___2,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_353_/1}).
-dialyzer({nowarn_function, yeccpars2_353_/1}).
-compile({nowarn_unused_function,  yeccpars2_353_/1}).
-file("src/compiler/parser/catena_parser.yrl", 412).
yeccpars2_353_(__Stack0) ->
 [___8,___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                                         
    {instance_decl,
        extract_atom(___4),
        ___5,
        ___2,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_355_/1}).
-dialyzer({nowarn_function, yeccpars2_355_/1}).
-compile({nowarn_unused_function,  yeccpars2_355_/1}).
-file("src/compiler/parser/catena_parser.yrl", 458).
yeccpars2_355_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_356_/1}).
-dialyzer({nowarn_function, yeccpars2_356_/1}).
-compile({nowarn_unused_function,  yeccpars2_356_/1}).
-file("src/compiler/parser/catena_parser.yrl", 256).
yeccpars2_356_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___2), "Malformed declaration before 'effect'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_357_/1}).
-dialyzer({nowarn_function, yeccpars2_357_/1}).
-compile({nowarn_unused_function,  yeccpars2_357_/1}).
-file("src/compiler/parser/catena_parser.yrl", 260).
yeccpars2_357_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    make_error_declaration(extract_location(___2), "Malformed declaration before 'instance'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_358_/1}).
-dialyzer({nowarn_function, yeccpars2_358_/1}).
-compile({nowarn_unused_function,  yeccpars2_358_/1}).
-file("src/compiler/parser/catena_parser.yrl", 258).
yeccpars2_358_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                            
    make_error_declaration(extract_location(___2), "Malformed declaration before 'trait'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_359_/1}).
-dialyzer({nowarn_function, yeccpars2_359_/1}).
-compile({nowarn_unused_function,  yeccpars2_359_/1}).
-file("src/compiler/parser/catena_parser.yrl", 254).
yeccpars2_359_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                
    make_error_declaration(extract_location(___2), "Malformed declaration before 'transform'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_360_/1}).
-dialyzer({nowarn_function, yeccpars2_360_/1}).
-compile({nowarn_unused_function,  yeccpars2_360_/1}).
-file("src/compiler/parser/catena_parser.yrl", 252).
yeccpars2_360_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___2), "Malformed declaration before 'type'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_361_/1}).
-dialyzer({nowarn_function, yeccpars2_361_/1}).
-compile({nowarn_unused_function,  yeccpars2_361_/1}).
-file("src/compiler/parser/catena_parser.yrl", 326).
yeccpars2_361_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___1), "Incomplete effect declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_362_/1}).
-dialyzer({nowarn_function, yeccpars2_362_/1}).
-compile({nowarn_unused_function,  yeccpars2_362_/1}).
-file("src/compiler/parser/catena_parser.yrl", 329).
yeccpars2_362_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_364_/1}).
-dialyzer({nowarn_function, yeccpars2_364_/1}).
-compile({nowarn_unused_function,  yeccpars2_364_/1}).
-file("src/compiler/parser/catena_parser.yrl", 329).
yeccpars2_364_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_366_/1}).
-dialyzer({nowarn_function, yeccpars2_366_/1}).
-compile({nowarn_unused_function,  yeccpars2_366_/1}).
-file("src/compiler/parser/catena_parser.yrl", 334).
yeccpars2_366_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {effect_operation,
        extract_atom(___2),
        undefined,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_368_/1}).
-dialyzer({nowarn_function, yeccpars2_368_/1}).
-compile({nowarn_unused_function,  yeccpars2_368_/1}).
-file("src/compiler/parser/catena_parser.yrl", 340).
yeccpars2_368_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_operation,
        extract_atom(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_369_/1}).
-dialyzer({nowarn_function, yeccpars2_369_/1}).
-compile({nowarn_unused_function,  yeccpars2_369_/1}).
-file("src/compiler/parser/catena_parser.yrl", 331).
yeccpars2_369_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_370_/1}).
-dialyzer({nowarn_function, yeccpars2_370_/1}).
-compile({nowarn_unused_function,  yeccpars2_370_/1}).
-file("src/compiler/parser/catena_parser.yrl", 319).
yeccpars2_370_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_decl,
        extract_atom(___2),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_371_/1}).
-dialyzer({nowarn_function, yeccpars2_371_/1}).
-compile({nowarn_unused_function,  yeccpars2_371_/1}).
-file("src/compiler/parser/catena_parser.yrl", 242).
yeccpars2_371_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_372_/1}).
-dialyzer({nowarn_function, yeccpars2_372_/1}).
-compile({nowarn_unused_function,  yeccpars2_372_/1}).
-file("src/compiler/parser/catena_parser.yrl", 222).
yeccpars2_372_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    ___1
  end | __Stack].

-compile({inline,yeccpars2_373_/1}).
-dialyzer({nowarn_function, yeccpars2_373_/1}).
-compile({nowarn_unused_function,  yeccpars2_373_/1}).
-file("src/compiler/parser/catena_parser.yrl", 225).
yeccpars2_373_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_374_/1}).
-dialyzer({nowarn_function, yeccpars2_374_/1}).
-compile({nowarn_unused_function,  yeccpars2_374_/1}).
-file("src/compiler/parser/catena_parser.yrl", 207).
yeccpars2_374_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {module_header, element(2, ___1), ___2, element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_380_/1}).
-dialyzer({nowarn_function, yeccpars2_380_/1}).
-compile({nowarn_unused_function,  yeccpars2_380_/1}).
-file("src/compiler/parser/catena_parser.yrl", 233).
yeccpars2_380_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {export_type, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_381_/1}).
-dialyzer({nowarn_function, yeccpars2_381_/1}).
-compile({nowarn_unused_function,  yeccpars2_381_/1}).
-file("src/compiler/parser/catena_parser.yrl", 235).
yeccpars2_381_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {export_transform, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_382_/1}).
-dialyzer({nowarn_function, yeccpars2_382_/1}).
-compile({nowarn_unused_function,  yeccpars2_382_/1}).
-file("src/compiler/parser/catena_parser.yrl", 231).
yeccpars2_382_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    {export_trait, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_383_/1}).
-dialyzer({nowarn_function, yeccpars2_383_/1}).
-compile({nowarn_unused_function,  yeccpars2_383_/1}).
-file("src/compiler/parser/catena_parser.yrl", 237).
yeccpars2_383_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {export_effect, extract_atom(___3)}
  end | __Stack].

-compile({inline,yeccpars2_384_/1}).
-dialyzer({nowarn_function, yeccpars2_384_/1}).
-compile({nowarn_unused_function,  yeccpars2_384_/1}).
-file("src/compiler/parser/catena_parser.yrl", 227).
yeccpars2_384_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_385_/1}).
-dialyzer({nowarn_function, yeccpars2_385_/1}).
-compile({nowarn_unused_function,  yeccpars2_385_/1}).
-file("src/compiler/parser/catena_parser.yrl", 193).
yeccpars2_385_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {module,
        element(2, ___1),   %% Module name
        element(3, ___1),   %% Exports
        [],                 %% Imports (future)
        ___2,               %% Declarations
        element(4, ___1)}
  end | __Stack].

-compile({inline,yeccpars2_386_/1}).
-dialyzer({nowarn_function, yeccpars2_386_/1}).
-compile({nowarn_unused_function,  yeccpars2_386_/1}).
-file("src/compiler/parser/catena_parser.yrl", 478).
yeccpars2_386_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {transform_decl,
        extract_transform_name(___1),
        extract_transform_type(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_387_/1}).
-dialyzer({nowarn_function, yeccpars2_387_/1}).
-compile({nowarn_unused_function,  yeccpars2_387_/1}).
-file("src/compiler/parser/catena_parser.yrl", 521).
yeccpars2_387_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_389_/1}).
-dialyzer({nowarn_function, yeccpars2_389_/1}).
-compile({nowarn_unused_function,  yeccpars2_389_/1}).
-file("src/compiler/parser/catena_parser.yrl", 567).
yeccpars2_389_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_395_/1}).
-dialyzer({nowarn_function, yeccpars2_395_/1}).
-compile({nowarn_unused_function,  yeccpars2_395_/1}).
-file("src/compiler/parser/catena_parser.yrl", 533).
yeccpars2_395_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                  
    {transform_clause,
        ___3,
        ___5,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_396_/1}).
-dialyzer({nowarn_function, yeccpars2_396_/1}).
-compile({nowarn_unused_function,  yeccpars2_396_/1}).
-file("src/compiler/parser/catena_parser.yrl", 526).
yeccpars2_396_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                    
    {transform_clause,
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_397_/1}).
-dialyzer({nowarn_function, yeccpars2_397_/1}).
-compile({nowarn_unused_function,  yeccpars2_397_/1}).
-file("src/compiler/parser/catena_parser.yrl", 523).
yeccpars2_397_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    [___1 | ___2]
  end | __Stack].


-file("src/compiler/parser/catena_parser.yrl", 1086).
