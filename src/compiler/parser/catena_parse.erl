-module(catena_parse).
-export([parse/1, parse_file/1, format_error/1]).
-export([
    get_max_ast_depth/0, get_max_ast_nodes/0,
    get_max_token_count/0, get_max_parse_time/0,
    get_max_pattern_depth/0, get_max_type_depth/0,
    %% Effect-specific resource limits
    get_max_effects_per_module/0,
    get_max_operations_per_effect/0,
    get_max_effects_in_annotation/0,
    get_max_effect_handler_depth/0,
    get_max_handlers_per_try/0,
    get_max_operations_per_handler/0,
    get_max_effect_identifier_length/0
]).

%%% @doc High-level parser wrapper for the Catena language.
%%%
%%% This module provides a convenient high-level API that combines lexical
%%% analysis (tokenization) and syntactic analysis (parsing) into a single
%%% operation with comprehensive resource protection and error handling.
%%%
%%% == Key Features ==
%%%
%%% <ul>
%%%   <li><b>Unified API</b> - Parse from strings or files with a single call</li>
%%%   <li><b>Resource Limits</b> - Six categories of protection against malicious input:
%%%     <ul>
%%%       <li>Token count limit (500k default)</li>
%%%       <li>Parse time limit (30s default)</li>
%%%       <li>AST depth limit (500 levels)</li>
%%%       <li>AST node count limit (100k nodes)</li>
%%%       <li>Pattern depth limit (100 levels)</li>
%%%       <li>Type depth limit (100 levels)</li>
%%%     </ul>
%%%   </li>
%%%   <li><b>Error Formatting</b> - Human-readable error messages with location info</li>
%%%   <li><b>Configurable Limits</b> - All limits adjustable via application environment</li>
%%% </ul>
%%%
%%% == Usage Example ==
%%%
%%% ```
%%% %% Parse from string
%%% {ok, AST} = catena_parse:parse("shape Maybe a = Some a | None"),
%%%
%%% %% Parse from file
%%% {ok, AST} = catena_parse:parse_file("example.cat"),
%%%
%%% %% Handle errors
%%% case catena_parse:parse(BadInput) of
%%%     {ok, AST} -> process(AST);
%%%     {error, Reason} ->
%%%         Msg = catena_parse:format_error(Reason),
%%%         io:format("Error: ~s~n", [Msg])
%%% end.
%%%
%%% %% Configure limits (in application environment)
%%% application:set_env(catena, max_ast_depth, 1000),
%%% application:set_env(catena, max_parse_time, 60000).
%%% '''
%%%
%%% @author Catena Contributors
%%% @version 1.0.0
%%% @since 2025-11-08

%% Default resource limits to prevent DoS attacks
%% These can be overridden via application environment:
%%   application:set_env(catena, max_ast_depth, NewDepth)
-define(DEFAULT_MAX_AST_DEPTH, 500).        % 500 levels - prevents stack overflow
-define(DEFAULT_MAX_AST_NODES, 100000).     % 100k nodes - reasonable for large files
-define(DEFAULT_MAX_TOKEN_COUNT, 500000).   % 500k tokens - reasonable upper bound

%% Effect-specific resource limits
-define(DEFAULT_MAX_EFFECTS_PER_MODULE, 50).      % Maximum effect declarations per module
-define(DEFAULT_MAX_OPERATIONS_PER_EFFECT, 100).  % Maximum operations per effect
-define(DEFAULT_MAX_EFFECTS_IN_ANNOTATION, 10).   % Maximum effects in type annotation
-define(DEFAULT_MAX_EFFECT_HANDLER_DEPTH, 20).    % Maximum nesting in effect handlers
-define(DEFAULT_MAX_HANDLERS_PER_TRY, 20).        % Maximum handlers in single try-with expression
-define(DEFAULT_MAX_OPERATIONS_PER_HANDLER, 50).  % Maximum operation cases per handler
-define(DEFAULT_MAX_EFFECT_IDENTIFIER_LENGTH, 100). % Maximum effect name length
-define(DEFAULT_MAX_PARSE_TIME, 30000).     % 30 seconds - prevent algorithmic attacks
-define(DEFAULT_MAX_PATTERN_DEPTH, 100).    % 100 levels - deeply nested patterns
-define(DEFAULT_MAX_TYPE_DEPTH, 100).       % 100 levels - deeply nested types

%% @doc Parse Catena source code into an Abstract Syntax Tree.
%%
%% Performs complete lexical and syntactic analysis in a single operation,
%% with comprehensive resource limit checking to prevent denial-of-service
%% attacks and resource exhaustion.
%%
%% The parsing process consists of 4 steps:
%% <ol>
%%%   <li>Tokenization using {@link catena_lexer}</li>
%%%   <li>Token count validation</li>
%%%   <li>Parsing with timeout protection using {@link catena_parser}</li>
%%%   <li>AST validation (depth, node count, pattern depth, type depth)</li>
%% </ol>
%%
%% All resource limits are configurable via application environment.
%%
%% @param String Catena source code as a string
%% @returns `{ok, AST}' on success, `{error, Reason}' on failure
%%
%% @see parse_file/1
%% @see format_error/1
%% @see catena_lexer:tokenize/1
%% @see catena_parser:parse/1
%%
%% @example
%% ```
%% %% Success case
%% {ok, AST} = parse("shape Maybe a = Some a | None").
%%%
%% %% Lexer error (invalid syntax)
%% {error, {lex_error, 1, "Illegal character: @"}} = parse("shape @Invalid").
%%%
%% %% Parser error (syntax error)
%% {error, {parse_error, 1, "unexpected token"}} = parse("shape =").
%%%
%% %% Resource limit exceeded
%% BigInput = string:copies("(", 10000),
%% {error, {ast_too_deep, _, _}} = parse(BigInput).
%% '''
-spec parse(string()) -> {ok, term()} | {error, term()}.
parse(String) ->
    StartTime = erlang:monotonic_time(millisecond),
    MaxParseTime = get_max_parse_time(),

    %% Step 1: Tokenize
    case catena_lexer:tokenize(String) of
        {ok, Tokens} ->
            %% Check token count limit
            TokenCount = length(Tokens),
            MaxTokens = get_max_token_count(),

            case TokenCount > MaxTokens of
                true ->
                    {error, {too_many_tokens, TokenCount, MaxTokens}};
                false ->
                    %% Step 2: Parse with timeout
                    case parse_with_timeout(Tokens, MaxParseTime, StartTime) of
                        {ok, AST} ->
                            %% Step 3: Validate AST resource limits
                            case validate_ast(AST) of
                                {ok, ValidatedAST} ->
                                    %% Step 4: Validate structural limits
                                    validate_structure(ValidatedAST);
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end
            end;
        {error, {Line, Module, Reason}} ->
            %% Standardize lexer errors
            {error, {lex_error, Line, format_lex_error(Module, Reason)}}
    end.

%% @doc Parse with timeout protection
-spec parse_with_timeout(list(), pos_integer(), integer()) -> {ok, term()} | {error, term()}.
parse_with_timeout(Tokens, MaxTime, StartTime) ->
    case catena_parser:parse(Tokens) of
        {ok, AST} ->
            ElapsedTime = erlang:monotonic_time(millisecond) - StartTime,
            case ElapsedTime > MaxTime of
                true ->
                    {error, {parse_timeout, ElapsedTime, MaxTime}};
                false ->
                    {ok, AST}
            end;
        {error, {Line, Module, Reason}} ->
            %% Check if we timed out
            ElapsedTime = erlang:monotonic_time(millisecond) - StartTime,
            case ElapsedTime > MaxTime of
                true ->
                    {error, {parse_timeout, ElapsedTime, MaxTime}};
                false ->
                    %% Normal parse error
                    {error, {parse_error, Line, format_parse_error(Module, Reason)}}
            end
    end.

%% @doc Parse a file containing Catena source code.
%%
%% Reads a file from disk and parses its contents using {@link parse/1}.
%% All resource limits and validation apply to the file contents.
%%
%% @param Filename Path to the file to parse (absolute or relative)
%% @returns `{ok, AST}' on success, `{error, Reason}' on failure
%%
%% @see parse/1
%% @see format_error/1
%%
%% @example
%% ```
%% %% Success case
%% {ok, AST} = parse_file("examples/maybe.cat").
%%%
%% %% File not found
%% {error, {file_error, "missing.cat", enoent}} =
%%     parse_file("missing.cat").
%%%
%% %% Parse error in file
%% {error, {parse_error, _, _}} = parse_file("invalid.cat").
%% '''
-spec parse_file(string()) -> {ok, term()} | {error, term()}.
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            parse(binary_to_list(Binary));
        {error, Reason} ->
            {error, {file_error, Filename, Reason}}
    end.

%% @doc Format error reason into human-readable string.
%%
%% Converts error tuples returned by {@link parse/1} and {@link parse_file/1}
%% into user-friendly error messages suitable for display.
%%
%% Handles all error types:
%% <ul>
%%%   <li>`{lex_error, Location, Reason}' - Lexical errors</li>
%%%   <li>`{parse_error, Location, Reason}' - Syntax errors</li>
%%%   <li>`{ast_too_deep, Depth, Max}' - AST depth exceeded</li>
%%%   <li>`{ast_too_large, Count, Max}' - AST node count exceeded</li>
%%%   <li>`{too_many_tokens, Count, Max}' - Token count exceeded</li>
%%%   <li>`{parse_timeout, Elapsed, Max}' - Parse timeout</li>
%%%   <li>`{pattern_too_deep, Depth, Max}' - Pattern nesting exceeded</li>
%%%   <li>`{type_too_deep, Depth, Max}' - Type nesting exceeded</li>
%%%   <li>`{file_error, Filename, Reason}' - File I/O errors</li>
%% </ul>
%%
%% @param Reason Error tuple from parse/1 or parse_file/1
%% @returns Formatted error message string
%%
%% @see parse/1
%% @see parse_file/1
%%
%% @example
%% ```
%% %% Format lexer error
%% "Lexical error at 5:10: Illegal character" =
%%     format_error({lex_error, {location, 5, 10}, "Illegal character"}).
%%%
%% %% Format parser error
%% "Parse error at line 3: unexpected token" =
%%     format_error({parse_error, 3, "unexpected token"}).
%%%
%% %% Format resource limit error
%% "AST too deep: 600 levels exceeds maximum of 500" =
%%     format_error({ast_too_deep, 600, 500}).
%% '''
-spec format_error(term()) -> string().
format_error({lex_error, Line, Reason}) when is_integer(Line) ->
    io_lib:format("Lexical error at line ~p: ~s", [Line, Reason]);
format_error({lex_error, Location, Reason}) ->
    LocStr = catena_location:format(Location),
    io_lib:format("Lexical error at ~s: ~s", [LocStr, Reason]);
format_error({parse_error, Line, Reason}) when is_integer(Line) ->
    io_lib:format("Parse error at line ~p: ~s", [Line, Reason]);

format_error({ast_too_deep, Depth, MaxDepth}) ->
    io_lib:format("AST too deep: ~p levels exceeds maximum of ~p",
                  [Depth, MaxDepth]);
format_error({ast_too_large, NodeCount, MaxNodes}) ->
    io_lib:format("AST too large: ~p nodes exceeds maximum of ~p",
                  [NodeCount, MaxNodes]);
format_error({too_many_tokens, Count, MaxCount}) ->
    io_lib:format("Too many tokens: ~p exceeds maximum of ~p",
                  [Count, MaxCount]);
format_error({parse_timeout, ElapsedTime, MaxTime}) ->
    io_lib:format("Parse timeout: took ~p ms, maximum is ~p ms",
                  [ElapsedTime, MaxTime]);
format_error({pattern_too_deep, Depth, MaxDepth}) ->
    io_lib:format("Pattern nesting too deep: ~p levels exceeds maximum of ~p",
                  [Depth, MaxDepth]);
format_error({type_too_deep, Depth, MaxDepth}) ->
    io_lib:format("Type expression too deep: ~p levels exceeds maximum of ~p",
                  [Depth, MaxDepth]);
format_error({file_error, Filename, Reason}) ->
    io_lib:format("Error reading file ~s: ~p", [Filename, Reason]);
%% Effect-specific error messages
format_error({too_many_effects, Count, Max}) ->
    io_lib:format("Too many effect declarations: ~p exceeds maximum of ~p per module",
                  [Count, Max]);
format_error({too_many_operations, EffectName, Count, Max}) ->
    io_lib:format("Effect '~w' has too many operations: ~p exceeds maximum of ~p",
                  [EffectName, Count, Max]);
format_error({effect_name_too_long, EffectName, Length, Max}) ->
    io_lib:format("Effect name '~w' is too long: ~p characters exceeds maximum of ~p",
                  [EffectName, Length, Max]);
format_error({operation_name_too_long, OpName, Length, Max}) ->
    io_lib:format("Operation name '~w' is too long: ~p characters exceeds maximum of ~p",
                  [OpName, Length, Max]);
format_error({too_many_effects_in_annotation, Count, Max}) ->
    io_lib:format("Effect annotation has too many effects: ~p exceeds maximum of ~p",
                  [Count, Max]);
format_error({effect_handler_too_deep, Depth, Max}) ->
    io_lib:format("Effect handler nesting too deep: ~p levels exceeds maximum of ~p",
                  [Depth, Max]).


%% @doc Get maximum AST depth limit.
%%
%% Returns the maximum allowed nesting depth for Abstract Syntax Trees.
%% Deeply nested ASTs can cause stack overflow; this limit prevents DoS attacks.
%% Default: 500 levels. Configurable via `application:set_env(catena, max_ast_depth, N)'.
%%
%% @returns Maximum AST depth in levels
%% @see catena_compiler_utils:get_max_ast_depth/0
-spec get_max_ast_depth() -> pos_integer().
get_max_ast_depth() ->
    application:get_env(catena, max_ast_depth, ?DEFAULT_MAX_AST_DEPTH).

%% @doc Get maximum AST node count limit.
%%
%% Returns the maximum allowed number of nodes in Abstract Syntax Trees.
%% Prevents memory exhaustion from pathologically large source files.
%% Default: 100,000 nodes. Configurable via `application:set_env(catena, max_ast_nodes, N)'.
%%
%% @returns Maximum AST node count
%% @see catena_compiler_utils:get_max_ast_nodes/0
-spec get_max_ast_nodes() -> pos_integer().
get_max_ast_nodes() ->
    application:get_env(catena, max_ast_nodes, ?DEFAULT_MAX_AST_NODES).

%% @doc Get maximum token count limit.
%%
%% Returns the maximum allowed number of tokens in a single parse operation.
%% Prevents excessive memory usage from very large token streams.
%% Default: 500,000 tokens. Configurable via `application:set_env(catena, max_token_count, N)'.
%%
%% @returns Maximum token count
%% @see catena_compiler_utils:get_max_token_count/0
-spec get_max_token_count() -> pos_integer().
get_max_token_count() ->
    application:get_env(catena, max_token_count, ?DEFAULT_MAX_TOKEN_COUNT).

%% @doc Get maximum parse time limit.
%%
%% Returns the maximum time allowed for a single parse operation in milliseconds.
%% Prevents algorithmic complexity attacks and runaway parsing.
%% Default: 30,000 ms (30 seconds). Configurable via `application:set_env(catena, max_parse_time, N)'.
%%
%% @returns Maximum parse time in milliseconds
%% @see catena_compiler_utils:get_max_parse_time/0
-spec get_max_parse_time() -> pos_integer().
get_max_parse_time() ->
    application:get_env(catena, max_parse_time, ?DEFAULT_MAX_PARSE_TIME).

%% @doc Get maximum pattern nesting depth limit.
%%
%% Returns the maximum allowed nesting depth for pattern matching expressions.
%% Deeply nested patterns can cause stack overflow during pattern compilation.
%% Default: 100 levels. Configurable via `application:set_env(catena, max_pattern_depth, N)'.
%%
%% @returns Maximum pattern depth in levels
%% @see catena_compiler_utils:get_max_pattern_depth/0
-spec get_max_pattern_depth() -> pos_integer().
get_max_pattern_depth() ->
    application:get_env(catena, max_pattern_depth, ?DEFAULT_MAX_PATTERN_DEPTH).

%% @doc Get maximum type expression depth limit.
%%
%% Returns the maximum allowed nesting depth for type expressions.
%% Deeply nested types can cause issues during type checking.
%% Default: 100 levels. Configurable via `application:set_env(catena, max_type_depth, N)'.
%%
%% @returns Maximum type depth in levels
%% @see catena_compiler_utils:get_max_type_depth/0
-spec get_max_type_depth() -> pos_integer().
get_max_type_depth() ->
    application:get_env(catena, max_type_depth, ?DEFAULT_MAX_TYPE_DEPTH).

%%============================================================================
%% Effect-Specific Resource Limits
%%============================================================================

%% @doc Get maximum effect declarations per module.
%%
%% Returns the maximum number of effect declarations allowed in a single module.
%% Prevents module bloat and compilation resource exhaustion.
%% Default: 50 effects. Configurable via `application:set_env(catena, max_effects_per_module, N)`.
%%
%% @returns Maximum effects per module
-spec get_max_effects_per_module() -> pos_integer().
get_max_effects_per_module() ->
    application:get_env(catena, max_effects_per_module, ?DEFAULT_MAX_EFFECTS_PER_MODULE).

%% @doc Get maximum operations per effect.
%%
%% Returns the maximum number of operations allowed in a single effect declaration.
%% Prevents complex effects that could cause compilation or runtime issues.
%% Default: 100 operations. Configurable via `application:set_env(catena, max_operations_per_effect, N)`.
%%
%% @returns Maximum operations per effect
-spec get_max_operations_per_effect() -> pos_integer().
get_max_operations_per_effect() ->
    application:get_env(catena, max_operations_per_effect, ?DEFAULT_MAX_OPERATIONS_PER_EFFECT).

%% @doc Get maximum effects in type annotation.
%%
%% Returns the maximum number of effects allowed in a single effect type annotation.
%% Prevents excessively complex effect sets that could overwhelm type inference.
%% Default: 10 effects. Configurable via `application:set_env(catena, max_effects_in_annotation, N)`.
%%
%% @returns Maximum effects in annotation
-spec get_max_effects_in_annotation() -> pos_integer().
get_max_effects_in_annotation() ->
    application:get_env(catena, max_effects_in_annotation, ?DEFAULT_MAX_EFFECTS_IN_ANNOTATION).

%% @doc Get maximum effect handler nesting depth.
%%
%% Returns the maximum allowed nesting depth for effect handlers in try-with expressions.
%% Prevents deep handler nesting that could cause stack overflow.
%% Default: 20 levels. Configurable via `application:set_env(catena, max_effect_handler_depth, N)`.
%%
%% @returns Maximum effect handler depth
-spec get_max_effect_handler_depth() -> pos_integer().
get_max_effect_handler_depth() ->
    application:get_env(catena, max_effect_handler_depth, ?DEFAULT_MAX_EFFECT_HANDLER_DEPTH).

%% @doc Get maximum handlers per try-with expression.
%%
%% Returns the maximum number of effect handlers allowed in a single try-with expression.
%% Prevents resource exhaustion from extremely large handler lists.
%% Default: 20 handlers. Configurable via `application:set_env(catena, max_handlers_per_try, N)`.
%%
%% @returns Maximum handlers per try-with
-spec get_max_handlers_per_try() -> pos_integer().
get_max_handlers_per_try() ->
    application:get_env(catena, max_handlers_per_try, ?DEFAULT_MAX_HANDLERS_PER_TRY).

%% @doc Get maximum operations per handler.
%%
%% Returns the maximum number of operation cases allowed in a single effect handler.
%% Prevents resource exhaustion from extremely large operation case lists.
%% Default: 50 operations. Configurable via `application:set_env(catena, max_operations_per_handler, N)`.
%%
%% @returns Maximum operations per handler
-spec get_max_operations_per_handler() -> pos_integer().
get_max_operations_per_handler() ->
    application:get_env(catena, max_operations_per_handler, ?DEFAULT_MAX_OPERATIONS_PER_HANDLER).

%% @doc Get maximum effect identifier length.
%%
%% Returns the maximum allowed length for effect names and operation identifiers.
%% Prevents excessively long identifiers that could cause memory or display issues.
%% Default: 100 characters. Configurable via `application:set_env(catena, max_effect_identifier_length, N)`.
%%
%% @returns Maximum effect identifier length
-spec get_max_effect_identifier_length() -> pos_integer().
get_max_effect_identifier_length() ->
    application:get_env(catena, max_effect_identifier_length, ?DEFAULT_MAX_EFFECT_IDENTIFIER_LENGTH).

%%============================================================================
%% Internal Functions
%%============================================================================

%% @doc Validate AST doesn't exceed resource limits
-spec validate_ast(term()) -> {ok, term()} | {error, term()}.
validate_ast(AST) ->
    MaxDepth = get_max_ast_depth(),
    MaxNodes = get_max_ast_nodes(),

    %% Calculate AST metrics
    Depth = calculate_ast_depth(AST),
    NodeCount = count_ast_nodes(AST),

    %% Check generic limits
    case {Depth > MaxDepth, NodeCount > MaxNodes} of
        {true, _} -> {error, {ast_too_deep, Depth, MaxDepth}};
        {_, true} -> {error, {ast_too_large, NodeCount, MaxNodes}};
        {false, false} -> 
            %% Check effect-specific limits
            validate_effect_limits(AST)
    end.

%% @doc Validate effect-specific resource limits
-spec validate_effect_limits(term()) -> {ok, term()} | {error, term()}.
validate_effect_limits(AST) ->
    case validate_effect_declarations(AST) of
        {ok, _} -> validate_effect_annotations(AST);
        Error -> Error
    end.

%% @doc Validate effect declarations against limits
-spec validate_effect_declarations(term()) -> {ok, term()} | {error, term()}.
validate_effect_declarations({module, _, _, _, Declarations, _}) ->
    MaxEffects = get_max_effects_per_module(),
    MaxOps = get_max_operations_per_effect(),
    MaxIdLength = get_max_effect_identifier_length(),
    
    EffectDecls = [Decl || Decl = {effect_decl, _, _, _} <- Declarations],
    EffectCount = length(EffectDecls),
    
    if
        EffectCount > MaxEffects ->
            {error, {too_many_effects, EffectCount, MaxEffects}};
        true ->
            validate_individual_effects(EffectDecls, MaxOps, MaxIdLength)
    end;
validate_effect_declarations(AST) ->
    {ok, AST}.  % Non-module AST, skip effect validation

%% @doc Validate individual effect declarations
-spec validate_individual_effects(list(), pos_integer(), pos_integer()) -> {ok, term()} | {error, term()}.
validate_individual_effects([], _MaxOps, _MaxIdLength) -> {ok, []};
validate_individual_effects([{effect_decl, Name, Operations, _} | Rest], MaxOps, MaxIdLength) ->
    %% Check effect name length
    case atom_length(Name) of
        Length when Length > MaxIdLength ->
            {error, {effect_name_too_long, Name, Length, MaxIdLength}};
        _ ->
            %% Check operation count
            OpCount = length(Operations),
            if
                OpCount > MaxOps ->
                    {error, {too_many_operations, Name, OpCount, MaxOps}};
                true ->
                    %% Check individual operation names
                    case validate_operations(Operations, MaxIdLength) of
                        {ok, _} -> validate_individual_effects(Rest, MaxOps, MaxIdLength);
                        Error -> Error
                    end
            end
    end.

%% @doc validate operation names within effects
-spec validate_operations(list(), pos_integer()) -> {ok, term()} | {error, term()}.
validate_operations([], _MaxIdLength) -> {ok, []};
validate_operations([{effect_operation, OpName, _Type, _} | Rest], MaxIdLength) ->
    case atom_length(OpName) of
        Length when Length > MaxIdLength ->
            {error, {operation_name_too_long, OpName, Length, MaxIdLength}};
        _ ->
            validate_operations(Rest, MaxIdLength)
    end;
validate_operations([_ | Rest], MaxIdLength) ->
    validate_operations(Rest, MaxIdLength).

%% @doc Validate effect annotations (type effect sets)
-spec validate_effect_annotations(term()) -> {ok, term()} | {error, term()}.
validate_effect_annotations({_ASTType, _, _, _, Declarations, _}) ->
    MaxEffectsAnnotation = get_max_effects_in_annotation(),
    validate_declarations_for_annotations(Declarations, MaxEffectsAnnotation);
validate_effect_annotations(AST) ->
    {ok, AST}.

%% @doc Validate declarations for effect annotations
-spec validate_declarations_for_annotations(list(), pos_integer()) -> {ok, term()} | {error, term()}.
validate_declarations_for_annotations([], _MaxEffects) -> {ok, []};
validate_declarations_for_annotations([Decl | Rest], MaxEffects) ->
    case validate_single_declaration_annotations(Decl, MaxEffects) of
        {ok, _} -> validate_declarations_for_annotations(Rest, MaxEffects);
        Error -> Error
    end.

%% @doc Validate effect annotations in a single declaration
-spec validate_single_declaration_annotations(term(), pos_integer()) -> {ok, term()} | {error, term()}.
validate_single_declaration_annotations({transform_decl, _Name, Type, _Clauses, _}, MaxEffects) ->
    validate_type_for_effects(Type, MaxEffects);
validate_single_declaration_annotations({type_decl, _Name, _Params, _Constructors, _}, _MaxEffects) ->
    {ok, {}};  % Type declarations don't have effect annotations
validate_single_declaration_annotations({effect_decl, _Name, _Operations, _}, _MaxEffects) ->
    {ok, {}};  % Effect declarations don't have effect annotations
validate_single_declaration_annotations(_, _MaxEffects) ->
    {ok, {}}.  % Other declaration types

%% @doc Validate type expressions for effect annotations
-spec validate_type_for_effects(term(), pos_integer()) -> {ok, term()} | {error, term()}.
validate_type_for_effects({type_effect, _BaseType, Effects, _}, MaxEffects) ->
    EffectCount = length(Effects),
    if
        EffectCount > MaxEffects ->
            {error, {too_many_effects_in_annotation, EffectCount, MaxEffects}};
        true ->
            {ok, Effects}
    end;
validate_type_for_effects(_OtherType, _MaxEffects) ->
    {ok, _OtherType}.

%% @doc Helper to get atom length safely
-spec atom_length(atom()) -> pos_integer().
atom_length(Atom) ->
    length(atom_to_list(Atom)).

%% @doc Validate structural limits (pattern depth, type depth)
-spec validate_structure(term()) -> {ok, term()} | {error, term()}.
validate_structure(AST) ->
    MaxPatternDepth = get_max_pattern_depth(),
    MaxTypeDepth = get_max_type_depth(),

    %% Check pattern depth
    PatternDepth = calculate_max_pattern_depth(AST),
    if
        PatternDepth > MaxPatternDepth ->
            {error, {pattern_too_deep, PatternDepth, MaxPatternDepth}};
        true ->
            %% Check type depth
            TypeDepth = calculate_max_type_depth(AST),
            if
                TypeDepth > MaxTypeDepth ->
                    {error, {type_too_deep, TypeDepth, MaxTypeDepth}};
                true ->
                    {ok, AST}
            end
    end.

%% @doc Calculate the maximum depth of an AST
-spec calculate_ast_depth(term()) -> non_neg_integer().
calculate_ast_depth(AST) ->
    calculate_depth(AST, 0).

calculate_depth(Tuple, CurrentDepth) when is_tuple(Tuple) ->
    %% For each element in the tuple, calculate depth and take max
    Elements = tuple_to_list(Tuple),
    ChildDepths = [calculate_depth(E, CurrentDepth + 1) || E <- Elements],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_depth(List, CurrentDepth) when is_list(List) ->
    %% For each element in the list, calculate depth and take max
    ChildDepths = [calculate_depth(E, CurrentDepth + 1) || E <- List],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_depth(_Atom, CurrentDepth) ->
    %% Atoms, integers, floats are leaves
    CurrentDepth.

%% @doc Count total number of nodes in AST
-spec count_ast_nodes(term()) -> non_neg_integer().
count_ast_nodes(Tuple) when is_tuple(Tuple) ->
    %% Count this node plus all children
    Elements = tuple_to_list(Tuple),
    1 + lists:sum([count_ast_nodes(E) || E <- Elements]);
count_ast_nodes(List) when is_list(List) ->
    %% Lists aren't nodes themselves, just containers
    lists:sum([count_ast_nodes(E) || E <- List]);
count_ast_nodes(_Leaf) ->
    %% Atoms, integers, floats, etc. are single nodes
    1.

%% @doc Calculate maximum pattern nesting depth in AST
-spec calculate_max_pattern_depth(term()) -> non_neg_integer().
calculate_max_pattern_depth(AST) ->
    calculate_pattern_depth(AST, 0).

calculate_pattern_depth({pat_constructor, _Name, Args, _Loc}, CurrentDepth) ->
    %% Nested pattern in constructor
    NewDepth = CurrentDepth + 1,
    ArgDepths = [calculate_pattern_depth(Arg, NewDepth) || Arg <- Args],
    case ArgDepths of
        [] -> NewDepth;
        _ -> lists:max(ArgDepths)
    end;
calculate_pattern_depth({pat_tuple, Elements, _Loc}, CurrentDepth) ->
    %% Nested patterns in tuple
    NewDepth = CurrentDepth + 1,
    ElemDepths = [calculate_pattern_depth(E, NewDepth) || E <- Elements],
    case ElemDepths of
        [] -> NewDepth;
        _ -> lists:max(ElemDepths)
    end;
calculate_pattern_depth({pat_list, Elements, _Loc}, CurrentDepth) ->
    %% Nested patterns in list
    NewDepth = CurrentDepth + 1,
    ElemDepths = [calculate_pattern_depth(E, NewDepth) || E <- Elements],
    case ElemDepths of
        [] -> NewDepth;
        _ -> lists:max(ElemDepths)
    end;
calculate_pattern_depth({pat_record, Fields, _Loc}, CurrentDepth) ->
    %% Nested patterns in record fields
    NewDepth = CurrentDepth + 1,
    FieldDepths = [calculate_pattern_depth(P, NewDepth) || {_, P} <- Fields],
    case FieldDepths of
        [] -> NewDepth;
        _ -> lists:max(FieldDepths)
    end;
calculate_pattern_depth({pat_var, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_pattern_depth({pat_wildcard, _}, CurrentDepth) ->
    CurrentDepth;
calculate_pattern_depth({pat_literal, _, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_pattern_depth(Tuple, CurrentDepth) when is_tuple(Tuple) ->
    %% Recursively check all tuple elements for patterns
    Elements = tuple_to_list(Tuple),
    ChildDepths = [calculate_pattern_depth(E, CurrentDepth) || E <- Elements],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_pattern_depth(List, CurrentDepth) when is_list(List) ->
    %% Recursively check all list elements for patterns
    ChildDepths = [calculate_pattern_depth(E, CurrentDepth) || E <- List],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_pattern_depth(_Other, CurrentDepth) ->
    CurrentDepth.

%% @doc Calculate maximum type expression depth in AST
-spec calculate_max_type_depth(term()) -> non_neg_integer().
calculate_max_type_depth(AST) ->
    calculate_type_depth(AST, 0).

calculate_type_depth({type_fun, From, To, _Loc}, CurrentDepth) ->
    %% Nested type in function type
    NewDepth = CurrentDepth + 1,
    FromDepth = calculate_type_depth(From, NewDepth),
    ToDepth = calculate_type_depth(To, NewDepth),
    max(FromDepth, ToDepth);
calculate_type_depth({type_app, Con, Args, _Loc}, CurrentDepth) ->
    %% Nested types in type application
    NewDepth = CurrentDepth + 1,
    ConDepth = calculate_type_depth(Con, NewDepth),
    ArgDepths = [calculate_type_depth(Arg, NewDepth) || Arg <- Args],
    AllDepths = [ConDepth | ArgDepths],
    lists:max(AllDepths);
calculate_type_depth({type_forall, _Vars, Type, _Loc}, CurrentDepth) ->
    %% Nested type in forall
    NewDepth = CurrentDepth + 1,
    calculate_type_depth(Type, NewDepth);
calculate_type_depth({type_tuple, Elements, _Loc}, CurrentDepth) ->
    %% Nested types in tuple
    NewDepth = CurrentDepth + 1,
    ElemDepths = [calculate_type_depth(E, NewDepth) || E <- Elements],
    case ElemDepths of
        [] -> NewDepth;
        _ -> lists:max(ElemDepths)
    end;
calculate_type_depth({type_record, Fields, _Extension, _Loc}, CurrentDepth) ->
    %% Nested types in record fields
    NewDepth = CurrentDepth + 1,
    FieldDepths = [calculate_type_depth(T, NewDepth) || {_, T} <- Fields],
    case FieldDepths of
        [] -> NewDepth;
        _ -> lists:max(FieldDepths)
    end;
calculate_type_depth({type_var, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_type_depth({type_con, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_type_depth(Tuple, CurrentDepth) when is_tuple(Tuple) ->
    %% Recursively check all tuple elements for types
    Elements = tuple_to_list(Tuple),
    ChildDepths = [calculate_type_depth(E, CurrentDepth) || E <- Elements],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_type_depth(List, CurrentDepth) when is_list(List) ->
    %% Recursively check all list elements for types
    ChildDepths = [calculate_type_depth(E, CurrentDepth) || E <- List],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_type_depth(_Other, CurrentDepth) ->
    CurrentDepth.

%% @doc Format lexer error messages
-spec format_lex_error(atom(), term()) -> string().
format_lex_error(catena_lexer, Reason) ->
    catena_lexer:format_error(Reason);
format_lex_error(_Module, Reason) when is_list(Reason) ->
    Reason;
format_lex_error(_Module, Reason) ->
    io_lib:format("~p", [Reason]).

%% @doc Format parser error messages
-spec format_parse_error(atom(), term()) -> string().
format_parse_error(catena_parser, Reason) when is_list(Reason) ->
    Reason;
format_parse_error(catena_parser, {unexpected, Token}) ->
    io_lib:format("unexpected token: ~p", [Token]);
format_parse_error(catena_parser, {expected, Expected, Got}) ->
    io_lib:format("expected ~p but got ~p", [Expected, Got]);
format_parse_error(_Module, Reason) when is_list(Reason) ->
    Reason;
format_parse_error(_Module, Reason) ->
    io_lib:format("~p", [Reason]).
