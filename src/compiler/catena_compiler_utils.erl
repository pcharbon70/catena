-module(catena_compiler_utils).

%%% @doc Shared utilities for the Catena compiler.
%%%
%%% This module provides common functionality used across all compiler phases:
%%%
%%% <ul>
%%%   <li><b>Token/AST Extraction</b> - Extract atoms, values, and locations from
%%%       tokens and AST nodes. Supports 40+ AST node types.</li>
%%%   <li><b>Configuration Management</b> - Centralized access to compiler configuration
%%%       with sensible defaults. All limits configurable via application environment.</li>
%%%   <li><b>Error Formatting</b> - Consistent, user-friendly error messages for
%%%       locations, file errors, size/depth limits, and timeouts.</li>
%%%   <li><b>AST Analysis</b> - Calculate depth, count nodes, analyze patterns and
%%%       types. Includes generic traversal functions (map/fold).</li>
%%%   <li><b>Validation</b> - Standard validation helpers for size, depth, and
%%%       timeout limits.</li>
%%% </ul>
%%%
%%% == Usage Example ==
%%%
%%% ```
%%% %% Extract from tokens
%%% Atom = catena_compiler_utils:extract_atom({lower_ident, 1, "foo"}),
%%% Loc = catena_compiler_utils:extract_location({transform, 5}),
%%%
%%% %% Get configuration
%%% MaxTokens = catena_compiler_utils:get_max_token_count(),
%%%
%%% %% Analyze AST
%%% Depth = catena_compiler_utils:ast_depth(AST),
%%% Count = catena_compiler_utils:ast_node_count(AST),
%%%
%%% %% Validate
%%% ok = catena_compiler_utils:validate_depth("AST", Depth, MaxDepth).
%%% '''
%%%
%%% @author Catena Contributors
%%% @version 1.0.0
%%% @since 2025-11-08

%%============================================================================
%% Exports
%%============================================================================

%% Token/AST extraction
-export([
    extract_atom/1,
    extract_value/1,
    extract_location/1,
    extract_name/1,
    extract_transform_name/1,
    extract_transform_type/1,
    extract_trait_constraint/1
]).

%% Configuration management
-export([
    get_config/2,
    get_config/3,
    get_max_input_size/0,
    get_max_nesting_depth/0,
    get_max_identifier_length/0,
    get_max_token_count/0,
    get_max_parse_time/0,
    get_max_ast_depth/0,
    get_max_ast_nodes/0,
    get_max_pattern_depth/0,
    get_max_type_depth/0,
    get_max_substitution_size/0,
    get_max_environment_size/0
]).

%% Error formatting
-export([
    format_location/1,
    format_file_error/1,
    format_size_error/3,
    format_depth_error/3,
    format_timeout_error/3
]).

%% AST utilities
-export([
    ast_depth/1,
    ast_node_count/1,
    pattern_depth/1,
    type_depth/1,
    ast_map/2,
    ast_fold/3
]).

%% Validation
-export([
    validate_size/3,
    validate_depth/3,
    validate_timeout/3
]).

%%============================================================================
%% Token/AST Extraction Functions
%%============================================================================

%% @doc Extract atom from token or AST node.
%%
%% Handles multiple token formats:
%% <ul>
%%%   <li>Tokens with atom values: `{lower_ident, 1, foo}'</li>
%%%   <li>Tokens with string values: `{lower_ident, 1, "bar"}'</li>
%%%   <li>Tag-only tokens: `{equals, 1}'</li>
%% </ul>
%%
%% @param Token A token tuple from the lexer or AST node
%% @returns The extracted atom
%%
%% @see extract_value/1
%% @see extract_location/1
%%
%% @example
%% ```
%% Atom1 = extract_atom({lower_ident, 1, foo}),      %% → foo
%% Atom2 = extract_atom({lower_ident, 1, "bar"}),    %% → bar
%% Atom3 = extract_atom({equals, 1}),                %% → equals
%% '''
-spec extract_atom(tuple()) -> atom().
extract_atom({_Tag, _Line, Atom}) when is_atom(Atom) -> Atom;
extract_atom({_Tag, _Line, String}) when is_list(String) -> list_to_atom(String);
extract_atom({Tag, _Line}) when is_atom(Tag) -> Tag.

%% @doc Extract value from token.
%%
%% Extracts the value field from tokens that carry values such as
%% integers, floats, and strings.
%%
%% @param Token A token tuple with a value field
%% @returns The extracted value (integer, float, string, etc.)
%%
%% @see extract_atom/1
%%
%% @example
%% ```
%% Val1 = extract_value({integer, 1, 42}),       %% → 42
%% Val2 = extract_value({float, 1, 3.14}),       %% → 3.14
%% Val3 = extract_value({string, 1, "hello"}),   %% → "hello"
%% '''
-spec extract_value(tuple()) -> term().
extract_value({_Tag, _Line, Value}) -> Value.

%% @doc Extract location from token or AST node.
%%
%% Intelligently extracts location information from over 40 different AST node
%% types and token formats. Supports both legacy `{line, N}' format and enhanced
%% `{location, Line, Col}' format from {@link catena_location}.
%%
%% Supported formats:
%% <ul>
%%%   <li>Simple tokens: `{transform, 5}' → `{location, 5, 0}'</li>
%%%   <li>Value tokens: `{integer, 10, 42}' → `{location, 10, 0}'</li>
%%%   <li>Expression nodes: `{var, x, Loc}' → `Loc'</li>
%%%   <li>Pattern nodes: `{pat_constructor, Name, Args, Loc}' → `Loc'</li>
%%%   <li>Type nodes: `{type_fun, From, To, Loc}' → `Loc'</li>
%%%   <li>Declaration nodes: `{transform_decl, Name, Type, Clauses, Loc}' → `Loc'</li>
%% </ul>
%%
%% @param Node A token or AST node containing location information
%% @returns Location tuple in {@link catena_location} format
%%
%% @see catena_location
%% @see catena_location:from_token/1
%%
%% @example
%% ```
%% %% From tokens
%% Loc1 = extract_location({transform, 5}),       %% → {location, 5, 0}
%% Loc2 = extract_location({integer, 10, 42}),    %% → {location, 10, 0}
%%%
%% %% From AST nodes
%% Loc3 = extract_location({var, x, {location, 3, 5}}),  %% → {location, 3, 5}
%% Loc4 = extract_location(BinaryOpNode),                %% → <location from node>
%% '''
-spec extract_location(tuple()) -> tuple().
extract_location({_Tag, Line}) when is_integer(Line) ->
    catena_location:from_token({_Tag, Line});
extract_location({_Tag, Line, _Value}) when is_integer(Line) ->
    catena_location:from_token({_Tag, Line, _Value});
extract_location({transform_sig, _Name, _Type, Loc}) -> Loc;
extract_location({var, _Name, Loc}) -> Loc;
extract_location({literal, _Value, _Type, Loc}) -> Loc;
extract_location({record_access, _Expr, _Field, Loc}) -> Loc;
extract_location({app, _Fun, _Args, Loc}) -> Loc;
extract_location({binary_op, _Op, _Left, _Right, Loc}) -> Loc;
extract_location({tuple_expr, _Elements, Loc}) -> Loc;
extract_location({list_expr, _Elements, Loc}) -> Loc;
extract_location({match_expr, _Clauses, Loc}) -> Loc;
extract_location({if_expr, _Cond, _Then, _Else, Loc}) -> Loc;
extract_location({let_expr, _Bindings, _Body, Loc}) -> Loc;
extract_location({match_clause, _Pattern, _Guard, _Body, Loc}) -> Loc;
extract_location({transform_clause, _Patterns, _Guard, _Body, Loc}) -> Loc;
extract_location({type_fun, _From, _To, Loc}) -> Loc;
extract_location({type_forall, _Vars, _Type, Loc}) -> Loc;
extract_location({type_app, _Con, _Args, Loc}) -> Loc;
extract_location({type_var, _Name, Loc}) -> Loc;
extract_location({type_con, _Name, Loc}) -> Loc;
extract_location({type_tuple, _Elements, Loc}) -> Loc;
extract_location({type_record, _Fields, _Extension, Loc}) -> Loc;
extract_location({pat_var, _Name, Loc}) -> Loc;
extract_location({pat_wildcard, Loc}) -> Loc;
extract_location({pat_constructor, _Name, _Args, Loc}) -> Loc;
extract_location({pat_literal, _Value, _Type, Loc}) -> Loc;
extract_location({pat_list, _Elements, Loc}) -> Loc;
extract_location({pat_tuple, _Elements, Loc}) -> Loc;
extract_location({pat_record, _Fields, Loc}) -> Loc;
extract_location({record_expr, _Fields, _Base, Loc}) -> Loc;
extract_location({type_decl, _Name, _Params, _Constructors, _Traits, Loc}) -> Loc;
extract_location({constructor, _Name, _Fields, Loc}) -> Loc;
extract_location({transform_decl, _Name, _Type, _Clauses, Loc}) -> Loc;
%% Effect syntax AST nodes
extract_location({effect_decl, _Name, _Operations, Loc}) -> Loc;
extract_location({effect_operation, _Name, _Type, Loc}) -> Loc;
extract_location({perform_expr, _Effect, _Operation, _Args, Loc}) -> Loc;
extract_location({try_with_expr, _Body, _Handlers, Loc}) -> Loc;
extract_location({handler_clause, _Effect, _Operations, Loc}) -> Loc;
extract_location({operation_case, _Operation, _Params, _Body, Loc}) -> Loc;
extract_location({type_effect, _Type, _Effects, Loc}) -> Loc;
extract_location(Tuple) when is_tuple(Tuple) ->
    %% Generic case: location is usually the last element
    Loc = element(tuple_size(Tuple), Tuple),
    case Loc of
        {line, _} -> Loc;
        {location, _, _} -> Loc;
        {location, _, _, _, _} -> Loc;
        _ when is_integer(Loc) -> catena_location:new(Loc, 0);
        _ -> {location, 1, 0}  % Fallback
    end.

%% @doc Extract name from AST node (generic).
%%
%% Attempts to extract the name field from various AST node formats.
%% This is a generic extraction function that handles multiple tuple structures
%% by checking for atom-valued name fields in common positions.
%%
%% Common AST node patterns:
%% <ul>
%%%   <li>5-tuples: `{tag, Name, _, _, _}'</li>
%%%   <li>4-tuples: `{tag, Name, _, _}'</li>
%%%   <li>3-tuples: `{tag, Name, _}'</li>
%% </ul>
%%
%% @param Node An AST node tuple
%% @returns The extracted name atom, or `undefined' if no name found
%%
%% @see extract_transform_name/1
%%
%% @example
%% ```
%% Name1 = extract_name({type_decl, 'Maybe', [], [], [], Loc}),   %% → 'Maybe'
%% Name2 = extract_name({var, x, Loc}),                           %% → x
%% Name3 = extract_name({literal, 42, integer, Loc}),             %% → undefined
%% '''
-spec extract_name(tuple()) -> atom() | undefined.
extract_name({_, Name, _, _, _}) when is_atom(Name) -> Name;
extract_name({_, Name, _, _}) when is_atom(Name) -> Name;
extract_name({_, Name, _}) when is_atom(Name) -> Name;
extract_name(_) -> undefined.

%% @doc Extract transform name from transform signature.
%%
%% Specialized extraction for transform signature nodes, which contain
%% the name and type of a transform (function) declaration.
%%
%% @param TransformSig A transform signature tuple `{transform_sig, Name, Type, Loc}'
%% @returns The transform name atom
%%
%% @see extract_transform_type/1
%% @see extract_name/1
%%
%% @example
%% ```
%% Name = extract_transform_name({transform_sig, map, TypeExpr, Loc}),  %% → map
%% '''
-spec extract_transform_name(tuple()) -> atom().
extract_transform_name({transform_sig, Name, _Type, _Loc}) -> Name.

%% @doc Extract transform type from transform signature.
%%
%% Extracts the type expression from a transform signature node.
%% The type expression describes the transform's parameter and return types.
%%
%% @param TransformSig A transform signature tuple `{transform_sig, Name, Type, Loc}'
%% @returns The type expression (typically a `type_fun' node)
%%
%% @see extract_transform_name/1
%%
%% @example
%% ```
%% %% For transform signature: map : (a -> b) -> List a -> List b
%% Type = extract_transform_type({transform_sig, map, TypeExpr, Loc}),
%% %% Type → {type_fun, ...}
%% '''
-spec extract_transform_type(tuple()) -> term().
extract_transform_type({transform_sig, _Name, Type, _Loc}) -> Type.

%% @doc Extract trait constraint from type expression.
%%
%% Converts type expressions to trait constraint records used in trait/instance
%% declarations. This helper is used by both the parser (for syntax construction)
%% and will be used by the type checker (for constraint resolution).
%%
%% Handles multiple type expression formats:
%% <ul>
%%%   <li>Type application: `{type_app, {type_con, Functor, L}, [f], L}' → `{trait_constraint, 'Functor', [f], L}'</li>
%%%   <li>Type constructor: `{type_con, Eq, L}' → `{trait_constraint, 'Eq', [], L}'</li>
%%%   <li>Invalid cases: Creates error constraint for validation phase</li>
%% </ul>
%%
%% @param TypeExpr A type expression AST node
%% @returns A `trait_constraint' record tuple
%%
%% @see catena_parser.yrl
%% @see catena_ast.hrl
%%
%% @example
%% ```
%% %% From "Functor f" in trait extends clause
%% C1 = extract_trait_constraint(
%%     {type_app, {type_con, 'Functor', Loc}, [{type_var, f, Loc2}], Loc}),
%% %% → {trait_constraint, 'Functor', [{type_var, f, Loc2}], Loc}
%%%
%% %% From "Eq" (no type args)
%% C2 = extract_trait_constraint({type_con, 'Eq', Loc}),
%% %% → {trait_constraint, 'Eq', [], Loc}
%%%
%% %% Invalid: type variable where trait name expected
%% C3 = extract_trait_constraint({type_var, a, Loc}),
%% %% → {trait_constraint, a, [], Loc}  (for error reporting)
%% '''
-spec extract_trait_constraint(tuple()) -> tuple().
extract_trait_constraint({type_app, {type_con, TraitName, Loc}, TypeArgs, _}) ->
    %% Type application: Functor f, Eq a, etc.
    {trait_constraint, TraitName, TypeArgs, Loc};
extract_trait_constraint({type_con, TraitName, Loc}) ->
    %% Simple type constructor: Eq, Show, etc. (no type arguments)
    {trait_constraint, TraitName, [], Loc};
extract_trait_constraint({type_var, VarName, Loc}) ->
    %% Invalid case: type variable used where trait name expected
    %% Create constraint for error reporting in validation phase
    {trait_constraint, VarName, [], Loc};
extract_trait_constraint(Other) ->
    %% Fallback for any other unexpected type expression
    %% Extract location if available, otherwise use default
    Loc = case Other of
        {_, _, L} -> L;
        {_, L} -> L;
        _ -> {location, 0, 0}
    end,
    {trait_constraint, 'InvalidTrait', [], Loc}.

%%============================================================================
%% Configuration Management
%%============================================================================

%% @doc Get configuration value from application environment.
%%
%% Retrieves a configuration value from the application environment.
%% Returns `undefined' if the key is not found.
%%
%% @param App The application name (typically `catena')
%% @param Key The configuration key atom
%% @returns The configuration value, or `undefined' if not found
%%
%% @see get_config/3
%% @see application:get_env/2
%%
%% @example
%% ```
%% Value = get_config(catena, max_token_count),  %% → 500000 or undefined
%% '''
-spec get_config(atom(), atom()) -> term() | undefined.
get_config(App, Key) ->
    application:get_env(App, Key, undefined).

%% @doc Get configuration value with default fallback.
%%
%% Retrieves a configuration value from the application environment.
%% Returns the specified default value if the key is not found.
%% This is the recommended way to access configuration with sensible defaults.
%%
%% @param App The application name (typically `catena')
%% @param Key The configuration key atom
%% @param Default The default value to return if key is not found
%% @returns The configuration value, or `Default' if not found
%%
%% @see get_config/2
%% @see application:get_env/3
%%
%% @example
%% ```
%% MaxTokens = get_config(catena, max_token_count, 500000),
%% %% → configured value or 500000
%% '''
-spec get_config(atom(), atom(), term()) -> term().
get_config(App, Key, Default) ->
    application:get_env(App, Key, Default).

%% Lexer configuration defaults
-define(DEFAULT_MAX_INPUT_SIZE, 10485760).        % 10MB
-define(DEFAULT_MAX_NESTING_DEPTH, 100).          % 100 levels
-define(DEFAULT_MAX_IDENTIFIER_LENGTH, 255).      % 255 chars

%% Parser configuration defaults
-define(DEFAULT_MAX_TOKEN_COUNT, 500000).         % 500k tokens
-define(DEFAULT_MAX_PARSE_TIME, 30000).           % 30 seconds
-define(DEFAULT_MAX_AST_DEPTH, 500).              % 500 levels
-define(DEFAULT_MAX_AST_NODES, 100000).           % 100k nodes
-define(DEFAULT_MAX_PATTERN_DEPTH, 100).          % 100 levels
-define(DEFAULT_MAX_TYPE_DEPTH, 100).             % 100 levels

%% Type system configuration defaults
-define(DEFAULT_MAX_SUBSTITUTION_SIZE, 10000).    % 10k substitutions
-define(DEFAULT_MAX_ENVIRONMENT_SIZE, 10000).     % 10k bindings

%% @doc Get maximum input size for lexer.
%%
%% Returns the maximum allowed size (in bytes) for source code input.
%% Default: 10MB (10,485,760 bytes).
%% Configurable via application environment key `max_input_size'.
%%
%% @returns Maximum input size in bytes
%%
%% @see catena_lexer
%%
%% @example
%% ```
%% MaxSize = get_max_input_size(),  %% → 10485760 (default) or configured value
%% '''
-spec get_max_input_size() -> pos_integer().
get_max_input_size() ->
    get_config(catena, max_input_size, ?DEFAULT_MAX_INPUT_SIZE).

%% @doc Get maximum nesting depth for lexer.
%%
%% Returns the maximum allowed nesting depth for lexical structures
%% such as nested comments, strings, or delimiters.
%% Default: 100 levels.
%% Configurable via application environment key `max_nesting_depth'.
%%
%% @returns Maximum nesting depth in levels
%%
%% @see catena_lexer
%%
%% @example
%% ```
%% MaxDepth = get_max_nesting_depth(),  %% → 100 (default) or configured value
%% '''
-spec get_max_nesting_depth() -> pos_integer().
get_max_nesting_depth() ->
    get_config(catena, max_nesting_depth, ?DEFAULT_MAX_NESTING_DEPTH).

%% @doc Get maximum identifier length for lexer.
%%
%% Returns the maximum allowed length for identifiers (variable names,
%% function names, etc.) in characters.
%% Default: 255 characters.
%% Configurable via application environment key `max_identifier_length'.
%%
%% @returns Maximum identifier length in characters
%%
%% @see catena_lexer
%%
%% @example
%% ```
%% MaxLen = get_max_identifier_length(),  %% → 255 (default) or configured value
%% '''
-spec get_max_identifier_length() -> pos_integer().
get_max_identifier_length() ->
    get_config(catena, max_identifier_length, ?DEFAULT_MAX_IDENTIFIER_LENGTH).

%% @doc Get maximum token count for parser.
%%
%% Returns the maximum number of tokens allowed in a single parse operation.
%% This prevents excessive memory usage from very large source files.
%% Default: 500,000 tokens.
%% Configurable via application environment key `max_token_count'.
%%
%% @returns Maximum token count
%%
%% @see catena_parse
%%
%% @example
%% ```
%% MaxTokens = get_max_token_count(),  %% → 500000 (default) or configured value
%% '''
-spec get_max_token_count() -> pos_integer().
get_max_token_count() ->
    get_config(catena, max_token_count, ?DEFAULT_MAX_TOKEN_COUNT).

%% @doc Get maximum parse time in milliseconds.
%%
%% Returns the maximum time allowed for a single parse operation.
%% This prevents infinite loops or exponential parsing complexity.
%% Default: 30,000 milliseconds (30 seconds).
%% Configurable via application environment key `max_parse_time'.
%%
%% @returns Maximum parse time in milliseconds
%%
%% @see catena_parse
%%
%% @example
%% ```
%% MaxTime = get_max_parse_time(),  %% → 30000 (default) or configured value
%% '''
-spec get_max_parse_time() -> pos_integer().
get_max_parse_time() ->
    get_config(catena, max_parse_time, ?DEFAULT_MAX_PARSE_TIME).

%% @doc Get maximum AST depth.
%%
%% Returns the maximum nesting depth allowed in the Abstract Syntax Tree.
%% This prevents stack overflow from deeply nested expressions.
%% Default: 500 levels.
%% Configurable via application environment key `max_ast_depth'.
%%
%% @returns Maximum AST depth in levels
%%
%% @see catena_parse
%% @see ast_depth/1
%%
%% @example
%% ```
%% MaxDepth = get_max_ast_depth(),  %% → 500 (default) or configured value
%% '''
-spec get_max_ast_depth() -> pos_integer().
get_max_ast_depth() ->
    get_config(catena, max_ast_depth, ?DEFAULT_MAX_AST_DEPTH).

%% @doc Get maximum AST node count.
%%
%% Returns the maximum number of nodes allowed in the Abstract Syntax Tree.
%% This prevents excessive memory usage from very large programs.
%% Default: 100,000 nodes.
%% Configurable via application environment key `max_ast_nodes'.
%%
%% @returns Maximum AST node count
%%
%% @see catena_parse
%% @see ast_node_count/1
%%
%% @example
%% ```
%% MaxNodes = get_max_ast_nodes(),  %% → 100000 (default) or configured value
%% '''
-spec get_max_ast_nodes() -> pos_integer().
get_max_ast_nodes() ->
    get_config(catena, max_ast_nodes, ?DEFAULT_MAX_AST_NODES).

%% @doc Get maximum pattern depth.
%%
%% Returns the maximum nesting depth allowed in pattern matching expressions.
%% This prevents stack overflow from deeply nested pattern destructuring.
%% Default: 100 levels.
%% Configurable via application environment key `max_pattern_depth'.
%%
%% @returns Maximum pattern depth in levels
%%
%% @see catena_parse
%% @see pattern_depth/1
%%
%% @example
%% ```
%% MaxDepth = get_max_pattern_depth(),  %% → 100 (default) or configured value
%% '''
-spec get_max_pattern_depth() -> pos_integer().
get_max_pattern_depth() ->
    get_config(catena, max_pattern_depth, ?DEFAULT_MAX_PATTERN_DEPTH).

%% @doc Get maximum type expression depth.
%%
%% Returns the maximum nesting depth allowed in type expressions.
%% This prevents stack overflow from deeply nested generic types.
%% Default: 100 levels.
%% Configurable via application environment key `max_type_depth'.
%%
%% @returns Maximum type depth in levels
%%
%% @see catena_parse
%% @see type_depth/1
%%
%% @example
%% ```
%% MaxDepth = get_max_type_depth(),  %% → 100 (default) or configured value
%% '''
-spec get_max_type_depth() -> pos_integer().
get_max_type_depth() ->
    get_config(catena, max_type_depth, ?DEFAULT_MAX_TYPE_DEPTH).

%% @doc Get maximum substitution size for type inference.
%%
%% Returns the maximum number of variable-to-type mappings allowed
%% in a single substitution. This prevents memory exhaustion from
%% excessively large substitutions during type inference.
%% Default: 10,000 substitutions.
%% Configurable via application environment key `max_substitution_size'.
%%
%% @returns Maximum substitution size
%%
%% @see catena_type_subst
%%
%% @example
%% ```
%% MaxSize = get_max_substitution_size(),  %% → 10000 (default) or configured value
%% '''
-spec get_max_substitution_size() -> pos_integer().
get_max_substitution_size() ->
    get_config(catena, max_substitution_size, ?DEFAULT_MAX_SUBSTITUTION_SIZE).

%% @doc Get maximum environment size for type inference.
%%
%% Returns the maximum number of variable-to-scheme bindings allowed
%% in a single type environment. This prevents memory exhaustion from
%% excessively large environments during type inference.
%% Default: 10,000 bindings.
%% Configurable via application environment key `max_environment_size'.
%%
%% @returns Maximum environment size
%%
%% @see catena_type_env
%%
%% @example
%% ```
%% MaxSize = get_max_environment_size(),  %% → 10000 (default) or configured value
%% '''
-spec get_max_environment_size() -> pos_integer().
get_max_environment_size() ->
    get_config(catena, max_environment_size, ?DEFAULT_MAX_ENVIRONMENT_SIZE).

%%============================================================================
%% Error Formatting
%%============================================================================

%% @doc Format location for error messages.
%%
%% Converts location information into a human-readable string format
%% suitable for error messages. Handles multiple location formats:
%% <ul>
%%%   <li>Raw line number: `5' → `"5"'</li>
%%%   <li>Legacy format: `{line, 5}' → `"5"'</li>
%%%   <li>Enhanced format: delegates to {@link catena_location:format/1}</li>
%% </ul>
%%
%% @param Location A location (integer, `{line, N}', or `{location, ...}' tuple)
%% @returns Formatted location string
%%
%% @see catena_location:format/1
%%
%% @example
%% ```
%% Msg1 = format_location(5),                        %% → "5"
%% Msg2 = format_location({line, 10}),               %% → "10"
%% Msg3 = format_location({location, 3, 5}),         %% → "3:5"
%% '''
-spec format_location(tuple() | integer()) -> string().
format_location(Line) when is_integer(Line) ->
    io_lib:format("~p", [Line]);
format_location({line, Line}) ->
    io_lib:format("~p", [Line]);
format_location(Loc) ->
    catena_location:format(Loc).

%% @doc Format file I/O error into user-friendly message.
%%
%% Converts POSIX file error atoms into readable error messages.
%% Handles common file system errors with clear explanations.
%%
%% Common error codes:
%% <ul>
%%%   <li>`enoent' - File not found</li>
%%%   <li>`eacces' - Permission denied</li>
%%%   <li>`eisdir' - Is a directory</li>
%%%   <li>`enotdir' - Not a directory</li>
%% </ul>
%%
%% @param Reason The error reason from `file:read_file/1' or similar
%% @returns User-friendly error message string
%%
%% @see file:read_file/1
%%
%% @example
%% ```
%% Msg1 = format_file_error(enoent),      %% → "File not found"
%% Msg2 = format_file_error(eacces),      %% → "Permission denied"
%% Msg3 = format_file_error(eisdir),      %% → "Is a directory"
%% '''
-spec format_file_error(term()) -> string().
format_file_error(enoent) -> "File not found";
format_file_error(eacces) -> "Permission denied";
format_file_error(eisdir) -> "Is a directory";
format_file_error(enotdir) -> "Not a directory";
format_file_error(Reason) when is_atom(Reason) ->
    atom_to_list(Reason);
format_file_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% @doc Format size/count exceeded error message.
%%
%% Creates a standardized error message when a size or count limit is exceeded.
%% Used for token count, node count, and similar numeric limits.
%%
%% @param What Description of what exceeded the limit (e.g., "Token count")
%% @param Actual The actual size/count that was encountered
%% @param Max The maximum allowed size/count
%% @returns Formatted error message string
%%
%% @see validate_size/3
%%
%% @example
%% ```
%% Msg = format_size_error("Token count", 600000, 500000),
%% %% → "Token count too large: 600000 (max 500000)"
%% '''
-spec format_size_error(string(), integer(), integer()) -> string().
format_size_error(What, Actual, Max) ->
    io_lib:format("~s too large: ~p (max ~p)", [What, Actual, Max]).

%% @doc Format depth exceeded error message.
%%
%% Creates a standardized error message when a nesting depth limit is exceeded.
%% Used for AST depth, pattern depth, type depth, and similar structural limits.
%%
%% @param What Description of what exceeded the depth (e.g., "AST depth")
%% @param Actual The actual depth that was encountered
%% @param Max The maximum allowed depth
%% @returns Formatted error message string
%%
%% @see validate_depth/3
%%
%% @example
%% ```
%% Msg = format_depth_error("AST depth", 550, 500),
%% %% → "AST depth too deep: 550 levels (max 500)"
%% '''
-spec format_depth_error(string(), integer(), integer()) -> string().
format_depth_error(What, Actual, Max) ->
    io_lib:format("~s too deep: ~p levels (max ~p)", [What, Actual, Max]).

%% @doc Format timeout error message.
%%
%% Creates a standardized error message when an operation exceeds time limit.
%% Used for parse timeouts and other time-bounded operations.
%%
%% @param What Description of what timed out (e.g., "Parse")
%% @param Elapsed The actual time elapsed in milliseconds
%% @param Max The maximum allowed time in milliseconds
%% @returns Formatted error message string
%%
%% @see validate_timeout/3
%%
%% @example
%% ```
%% Msg = format_timeout_error("Parse", 45000, 30000),
%% %% → "Parse timeout: exceeded 30000ms (took 45000ms)"
%% '''
-spec format_timeout_error(string(), integer(), integer()) -> string().
format_timeout_error(What, Elapsed, Max) ->
    io_lib:format("~s timeout: exceeded ~pms (took ~pms)", [What, Max, Elapsed]).

%%============================================================================
%% AST Utilities
%%============================================================================

%% @doc Calculate maximum nesting depth of AST.
%%
%% Recursively traverses the entire AST to find the maximum nesting depth.
%% Useful for detecting deeply nested structures that might cause stack overflow
%% or exceed resource limits.
%%
%% The depth is calculated as:
%% <ul>
%%%   <li>Literal nodes have depth 1</li>
%%%   <li>Compound nodes add 1 to the maximum depth of their children</li>
%%%   <li>The deepest path from root to leaf determines the total depth</li>
%% </ul>
%%
%% @param AST The AST node to analyze (can be any AST node type)
%% @returns The maximum nesting depth as a non-negative integer
%%
%% @see ast_node_count/1
%% @see pattern_depth/1
%% @see type_depth/1
%%
%% @example
%% ```
%% %% Simple literal: depth = 1
%% Depth1 = ast_depth({literal, 42, integer, Loc}),  %% → 1
%%%
%% %% Binary operation: depth = 2
%% Depth2 = ast_depth({binary_op, plus,
%%     {literal, 1, integer, L1},
%%     {literal, 2, integer, L2},
%%     Loc}),  %% → 2
%%%
%% %% Nested operations: ((1 + 2) + 3) depth = 3
%% Depth3 = ast_depth({binary_op, plus,
%%     {binary_op, plus,
%%         {literal, 1, integer, L1},
%%         {literal, 2, integer, L2},
%%         L3},
%%     {literal, 3, integer, L4},
%%     Loc}),  %% → 3
%% '''
-spec ast_depth(tuple()) -> non_neg_integer().
ast_depth(AST) ->
    ast_depth(AST, 0).

ast_depth({module, _, _, _, Decls, _}, CurrentDepth) ->
    lists:max([0 | [ast_depth(D, CurrentDepth + 1) || D <- Decls]]);
ast_depth({type_decl, _, _, Constructors, _, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(C, CurrentDepth + 1) || C <- Constructors]]);
ast_depth({constructor, _, Fields, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(F, CurrentDepth + 1) || F <- Fields]]);
ast_depth({transform_decl, _, Type, Clauses, _}, CurrentDepth) ->
    TypeDepth = case Type of
        undefined -> CurrentDepth;
        _ -> ast_depth(Type, CurrentDepth + 1)
    end,
    ClauseDepths = [ast_depth(C, CurrentDepth + 1) || C <- Clauses],
    lists:max([TypeDepth | ClauseDepths]);
ast_depth({transform_clause, Patterns, Guards, Body, _}, CurrentDepth) ->
    PatternDepths = [ast_depth(P, CurrentDepth + 1) || P <- Patterns],
    GuardDepths = case Guards of
        undefined -> [CurrentDepth];
        _ -> [ast_depth(G, CurrentDepth + 1) || G <- Guards]
    end,
    BodyDepth = ast_depth(Body, CurrentDepth + 1),
    lists:max([BodyDepth | PatternDepths ++ GuardDepths]);
ast_depth({match_expr, Clauses, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(C, CurrentDepth + 1) || C <- Clauses]]);
ast_depth({match_clause, Pattern, Guards, Body, _}, CurrentDepth) ->
    PatternDepth = ast_depth(Pattern, CurrentDepth + 1),
    GuardDepths = case Guards of
        undefined -> [CurrentDepth];
        _ -> [ast_depth(G, CurrentDepth + 1) || G <- Guards]
    end,
    BodyDepth = ast_depth(Body, CurrentDepth + 1),
    lists:max([PatternDepth, BodyDepth | GuardDepths]);
ast_depth({binary_op, _, Left, Right, _}, CurrentDepth) ->
    max(ast_depth(Left, CurrentDepth + 1), ast_depth(Right, CurrentDepth + 1));
ast_depth({app, Fun, Args, _}, CurrentDepth) ->
    FunDepth = ast_depth(Fun, CurrentDepth + 1),
    ArgDepths = [ast_depth(A, CurrentDepth + 1) || A <- Args],
    lists:max([FunDepth | ArgDepths]);
ast_depth({if_expr, Cond, Then, Else, _}, CurrentDepth) ->
    lists:max([ast_depth(Cond, CurrentDepth + 1),
               ast_depth(Then, CurrentDepth + 1),
               ast_depth(Else, CurrentDepth + 1)]);
ast_depth({let_expr, Bindings, Body, _}, CurrentDepth) ->
    BindingDepths = [max(ast_depth(P, CurrentDepth + 1), ast_depth(E, CurrentDepth + 1))
                     || {P, E} <- Bindings],
    BodyDepth = ast_depth(Body, CurrentDepth + 1),
    lists:max([BodyDepth | BindingDepths]);
ast_depth({tuple_expr, Elements, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(E, CurrentDepth + 1) || E <- Elements]]);
ast_depth({list_expr, Elements, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(E, CurrentDepth + 1) || E <- Elements]]);
ast_depth({record_expr, Fields, Base, _}, CurrentDepth) ->
    FieldDepths = [ast_depth(E, CurrentDepth + 1) || {_, E} <- Fields],
    BaseDepth = case Base of
        undefined -> CurrentDepth;
        _ -> ast_depth(Base, CurrentDepth + 1)
    end,
    lists:max([BaseDepth | FieldDepths]);
ast_depth({record_access, Expr, _, _}, CurrentDepth) ->
    ast_depth(Expr, CurrentDepth + 1);
ast_depth({pat_constructor, _, Args, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(A, CurrentDepth + 1) || A <- Args]]);
ast_depth({pat_tuple, Elements, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(E, CurrentDepth + 1) || E <- Elements]]);
ast_depth({pat_list, Elements, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(E, CurrentDepth + 1) || E <- Elements]]);
ast_depth({pat_record, Fields, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(P, CurrentDepth + 1) || {_, P} <- Fields]]);
ast_depth({type_fun, From, To, _}, CurrentDepth) ->
    max(ast_depth(From, CurrentDepth + 1), ast_depth(To, CurrentDepth + 1));
ast_depth({type_forall, _, Type, _}, CurrentDepth) ->
    ast_depth(Type, CurrentDepth + 1);
ast_depth({type_app, Con, Args, _}, CurrentDepth) ->
    ConDepth = ast_depth(Con, CurrentDepth + 1),
    ArgDepths = [ast_depth(A, CurrentDepth + 1) || A <- Args],
    lists:max([ConDepth | ArgDepths]);
ast_depth({type_tuple, Elements, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(E, CurrentDepth + 1) || E <- Elements]]);
ast_depth({type_record, Fields, _, _}, CurrentDepth) ->
    lists:max([CurrentDepth | [ast_depth(T, CurrentDepth + 1) || {_, T} <- Fields]]);
ast_depth(_, CurrentDepth) ->
    CurrentDepth + 1.

%% @doc Count total number of AST nodes.
%%
%% Traverses the entire AST and counts all nodes, including the root.
%% Useful for detecting excessively large programs that might consume
%% too much memory or processing time.
%%
%% Implementation uses {@link ast_fold/3} for efficient traversal.
%%
%% @param AST The AST node to count (can be any AST node type)
%% @returns Total number of nodes in the tree
%%
%% @see ast_depth/1
%% @see ast_fold/3
%% @see get_max_ast_nodes/0
%%
%% @example
%% ```
%% %% Simple literal: count = 1
%% Count1 = ast_node_count({literal, 42, integer, Loc}),  %% → 1
%%%
%% %% Binary operation: count = 3 (op + 2 literals)
%% Count2 = ast_node_count({binary_op, plus,
%%     {literal, 1, integer, L1},
%%     {literal, 2, integer, L2},
%%     Loc}),  %% → 3
%% '''
-spec ast_node_count(tuple()) -> non_neg_integer().
ast_node_count(AST) ->
    ast_fold(fun(_, Acc) -> Acc + 1 end, 0, AST).

%% @doc Calculate maximum pattern matching nesting depth.
%%
%% Recursively analyzes pattern matching expressions to find the maximum
%% nesting depth. This is important for preventing stack overflow during
%% pattern compilation and match execution.
%%
%% Depth is calculated for:
%% <ul>
%%%   <li>Constructor patterns: `Some (Some x)' has depth 2</li>
%%%   <li>Tuple patterns: `(1, (2, 3))' has depth 2</li>
%%%   <li>List patterns: `[1, [2, 3]]' has depth 2</li>
%%%   <li>Record patterns: `{x: {y: z}}' has depth 2</li>
%% </ul>
%%
%% @param AST The AST node to analyze (pattern or declaration containing patterns)
%% @returns Maximum pattern nesting depth
%%
%% @see ast_depth/1
%% @see type_depth/1
%% @see get_max_pattern_depth/0
%%
%% @example
%% ```
%% %% Simple variable pattern: depth = 0
%% Depth1 = pattern_depth({pat_var, x, Loc}),  %% → 0
%%%
%% %% Constructor pattern: Some x, depth = 1
%% Depth2 = pattern_depth({pat_constructor, 'Some',
%%     [{pat_var, x, L1}], Loc}),  %% → 1
%%%
%% %% Nested constructor: Some (Some x), depth = 2
%% Depth3 = pattern_depth({pat_constructor, 'Some',
%%     [{pat_constructor, 'Some', [{pat_var, x, L1}], L2}],
%%     Loc}),  %% → 2
%% '''
-spec pattern_depth(tuple()) -> non_neg_integer().
pattern_depth(AST) ->
    pattern_depth(AST, 0).

pattern_depth({pat_constructor, _, Args, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    case Args of
        [] -> NewDepth;
        _ -> lists:max([pattern_depth(Arg, NewDepth) || Arg <- Args])
    end;
pattern_depth({pat_tuple, Elements, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    case Elements of
        [] -> NewDepth;
        _ -> lists:max([pattern_depth(E, NewDepth) || E <- Elements])
    end;
pattern_depth({pat_list, Elements, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    case Elements of
        [] -> NewDepth;
        _ -> lists:max([pattern_depth(E, NewDepth) || E <- Elements])
    end;
pattern_depth({pat_record, Fields, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    case Fields of
        [] -> NewDepth;
        _ -> lists:max([pattern_depth(P, NewDepth) || {_, P} <- Fields])
    end;
pattern_depth({transform_clause, Patterns, _, _, _}, CurrentDepth) ->
    case Patterns of
        [] -> CurrentDepth;
        _ -> lists:max([pattern_depth(P, CurrentDepth) || P <- Patterns])
    end;
pattern_depth({transform_decl, _, _, Clauses, _}, CurrentDepth) ->
    case Clauses of
        [] -> CurrentDepth;
        _ -> lists:max([pattern_depth(C, CurrentDepth) || C <- Clauses])
    end;
pattern_depth({module, _, _, _, Decls, _}, CurrentDepth) ->
    case Decls of
        [] -> CurrentDepth;
        _ -> lists:max([pattern_depth(D, CurrentDepth) || D <- Decls])
    end;
pattern_depth(_, CurrentDepth) ->
    CurrentDepth.

%% @doc Calculate maximum type expression nesting depth.
%%
%% Recursively analyzes type expressions to find the maximum nesting depth.
%% This is important for preventing stack overflow during type checking and
%% for limiting complexity of generic type instantiation.
%%
%% Depth is calculated for:
%% <ul>
%%%   <li>Function types: `a -> b -> c' has depth 1</li>
%%%   <li>Type application: `List (Maybe Int)' has depth 2</li>
%%%   <li>Forall quantification: `forall a. f a' has depth 1</li>
%%%   <li>Tuple types: `(Int, (String, Bool))' has depth 2</li>
%% </ul>
%%
%% @param AST The AST node to analyze (type expression or declaration with types)
%% @returns Maximum type expression depth
%%
%% @see ast_depth/1
%% @see pattern_depth/1
%% @see get_max_type_depth/0
%%
%% @example
%% ```
%% %% Simple type variable: depth = 0
%% Depth1 = type_depth({type_var, a, Loc}),  %% → 0
%%%
%% %% Function type: a -> b, depth = 1
%% Depth2 = type_depth({type_fun,
%%     {type_var, a, L1},
%%     {type_var, b, L2},
%%     Loc}),  %% → 1
%%%
%% %% Nested application: List (Maybe Int), depth = 2
%% Depth3 = type_depth({type_app,
%%     {type_con, 'List', L1},
%%     [{type_app, {type_con, 'Maybe', L2}, [{type_con, 'Int', L3}], L4}],
%%     Loc}),  %% → 2
%% '''
-spec type_depth(tuple()) -> non_neg_integer().
type_depth(AST) ->
    type_depth(AST, 0).

type_depth({type_fun, From, To, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    max(type_depth(From, NewDepth), type_depth(To, NewDepth));
type_depth({type_forall, _, Type, _}, CurrentDepth) ->
    type_depth(Type, CurrentDepth + 1);
type_depth({type_app, Con, Args, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    ConDepth = type_depth(Con, NewDepth),
    case Args of
        [] -> ConDepth;
        _ -> lists:max([ConDepth | [type_depth(A, NewDepth) || A <- Args]])
    end;
type_depth({type_tuple, Elements, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    case Elements of
        [] -> NewDepth;
        _ -> lists:max([type_depth(E, NewDepth) || E <- Elements])
    end;
type_depth({type_record, Fields, _, _}, CurrentDepth) ->
    NewDepth = CurrentDepth + 1,
    case Fields of
        [] -> NewDepth;
        _ -> lists:max([type_depth(T, NewDepth) || {_, T} <- Fields])
    end;
type_depth({transform_decl, _, Type, _, _}, CurrentDepth) ->
    case Type of
        undefined -> CurrentDepth;
        _ -> type_depth(Type, CurrentDepth)
    end;
type_depth({type_decl, _, _, Constructors, _, _}, CurrentDepth) ->
    case Constructors of
        [] -> CurrentDepth;
        _ -> lists:max([type_depth(C, CurrentDepth) || C <- Constructors])
    end;
type_depth({constructor, _, Fields, _}, CurrentDepth) ->
    case Fields of
        [] -> CurrentDepth;
        _ -> lists:max([type_depth(F, CurrentDepth) || F <- Fields])
    end;
type_depth({module, _, _, _, Decls, _}, CurrentDepth) ->
    case Decls of
        [] -> CurrentDepth;
        _ -> lists:max([type_depth(D, CurrentDepth) || D <- Decls])
    end;
type_depth(_, CurrentDepth) ->
    CurrentDepth.

%% @doc Map a transformation function over all AST nodes.
%%
%% Performs a top-down traversal of the AST, applying the given function
%% to each node. The function is applied to the current node first, then
%% recursively to all children. This enables AST transformations and
%% optimizations.
%%
%% The transformation function receives a node and must return a
%% (possibly modified) node of the same type.
%%
%% @param Fun Transformation function `(Node :: tuple()) -> tuple()'
%% @param AST The AST to transform
%% @returns Transformed AST
%%
%% @see ast_fold/3
%%
%% @example
%% ```
%% %% Example: Replace all integer literals with 0
%% ReplaceLiterals = fun
%%     ({literal, _, integer, Loc}) -> {literal, 0, integer, Loc};
%%     (Other) -> Other
%% end,
%% NewAST = ast_map(ReplaceLiterals, AST).
%% '''
-spec ast_map(fun((tuple()) -> tuple()), tuple()) -> tuple().
ast_map(Fun, AST) when is_tuple(AST) ->
    %% Apply function to current node
    AST1 = Fun(AST),
    %% Recursively apply to children
    ast_map_children(Fun, AST1);
ast_map(_Fun, AST) ->
    AST.

ast_map_children(Fun, {module, Name, Exports, Imports, Decls, Loc}) ->
    {module, Name, Exports, Imports, [ast_map(Fun, D) || D <- Decls], Loc};
ast_map_children(Fun, {transform_decl, Name, Type, Clauses, Loc}) ->
    Type1 = case Type of
        undefined -> undefined;
        _ -> ast_map(Fun, Type)
    end,
    {transform_decl, Name, Type1, [ast_map(Fun, C) || C <- Clauses], Loc};
ast_map_children(_Fun, AST) ->
    %% Add more cases as needed
    AST.

%% @doc Fold an accumulator function over all AST nodes.
%%
%% Performs a depth-first traversal of the AST, threading an accumulator
%% through all nodes. The function is applied to the current node first,
%% then recursively to all children with the updated accumulator.
%%
%% This is useful for:
%% <ul>
%%%   <li>Collecting information (e.g., counting nodes, gathering names)</li>
%%%   <li>Validating properties (e.g., checking for undefined variables)</li>
%%%   <li>Building indices or symbol tables</li>
%% </ul>
%%
%% @param Fun Accumulator function `(Node :: tuple(), Acc) -> NewAcc'
%% @param Acc Initial accumulator value
%% @param AST The AST to fold over
%% @returns Final accumulator value
%%
%% @see ast_map/2
%% @see ast_node_count/1
%%
%% @example
%% ```
%% %% Example: Count all variable references
%% CountVars = fun
%%     ({var, _, _}, Acc) -> Acc + 1;
%%     (_, Acc) -> Acc
%% end,
%% VarCount = ast_fold(CountVars, 0, AST).
%%%
%% %% Example: Collect all function names
%% CollectNames = fun
%%     ({transform_decl, Name, _, _, _}, Acc) -> [Name | Acc];
%%     (_, Acc) -> Acc
%% end,
%% Names = ast_fold(CollectNames, [], AST).
%% '''
-spec ast_fold(fun((tuple(), Acc) -> Acc), Acc, tuple()) -> Acc when Acc :: term().
ast_fold(Fun, Acc, AST) when is_tuple(AST) ->
    %% Apply function to current node
    Acc1 = Fun(AST, Acc),
    %% Recursively fold over children
    ast_fold_children(Fun, Acc1, AST);
ast_fold(_Fun, Acc, _AST) ->
    Acc.

ast_fold_children(Fun, Acc, {module, _, _, _, Decls, _}) ->
    lists:foldl(fun(D, A) -> ast_fold(Fun, A, D) end, Acc, Decls);
ast_fold_children(Fun, Acc, {type_decl, _, _, Constructors, _, _}) ->
    lists:foldl(fun(C, A) -> ast_fold(Fun, A, C) end, Acc, Constructors);
ast_fold_children(Fun, Acc, {constructor, _, Fields, _}) ->
    lists:foldl(fun(F, A) -> ast_fold(Fun, A, F) end, Acc, Fields);
ast_fold_children(Fun, Acc, {transform_decl, _, Type, Clauses, _}) ->
    Acc1 = case Type of
        undefined -> Acc;
        _ -> ast_fold(Fun, Acc, Type)
    end,
    lists:foldl(fun(C, A) -> ast_fold(Fun, A, C) end, Acc1, Clauses);
ast_fold_children(Fun, Acc, {transform_clause, Patterns, Guards, Body, _}) ->
    Acc1 = lists:foldl(fun(P, A) -> ast_fold(Fun, A, P) end, Acc, Patterns),
    Acc2 = case Guards of
        undefined -> Acc1;
        _ -> lists:foldl(fun(G, A) -> ast_fold(Fun, A, G) end, Acc1, Guards)
    end,
    ast_fold(Fun, Acc2, Body);
ast_fold_children(Fun, Acc, {binary_op, _, Left, Right, _}) ->
    Acc1 = ast_fold(Fun, Acc, Left),
    ast_fold(Fun, Acc1, Right);
ast_fold_children(Fun, Acc, {app, Func, Args, _}) ->
    Acc1 = ast_fold(Fun, Acc, Func),
    lists:foldl(fun(A, Acc0) -> ast_fold(Fun, Acc0, A) end, Acc1, Args);
ast_fold_children(Fun, Acc, {tuple_expr, Elements, _}) ->
    lists:foldl(fun(E, A) -> ast_fold(Fun, A, E) end, Acc, Elements);
ast_fold_children(Fun, Acc, {list_expr, Elements, _}) ->
    lists:foldl(fun(E, A) -> ast_fold(Fun, A, E) end, Acc, Elements);
ast_fold_children(Fun, Acc, {pat_constructor, _, Args, _}) ->
    lists:foldl(fun(A, Acc0) -> ast_fold(Fun, Acc0, A) end, Acc, Args);
ast_fold_children(Fun, Acc, {pat_tuple, Elements, _}) ->
    lists:foldl(fun(E, A) -> ast_fold(Fun, A, E) end, Acc, Elements);
ast_fold_children(Fun, Acc, {pat_list, Elements, _}) ->
    lists:foldl(fun(E, A) -> ast_fold(Fun, A, E) end, Acc, Elements);
ast_fold_children(Fun, Acc, {type_fun, From, To, _}) ->
    Acc1 = ast_fold(Fun, Acc, From),
    ast_fold(Fun, Acc1, To);
ast_fold_children(Fun, Acc, {type_app, Con, Args, _}) ->
    Acc1 = ast_fold(Fun, Acc, Con),
    lists:foldl(fun(A, Acc0) -> ast_fold(Fun, Acc0, A) end, Acc1, Args);
ast_fold_children(_Fun, Acc, _AST) ->
    Acc.

%%============================================================================
%% Validation Utilities
%%============================================================================

%% @doc Validate size or count against maximum limit.
%%
%% Checks if an actual size/count value exceeds the maximum allowed.
%% Returns `ok' if within limits, or an error tuple if exceeded.
%%
%% The error tuple format is: `{error, {size_exceeded, What, Actual, Max}}'
%%
%% Common uses:
%% <ul>
%%%   <li>Token count validation (parser)</li>
%%%   <li>AST node count validation (parser)</li>
%%%   <li>Input size validation (lexer)</li>
%%%   <li>Identifier length validation (lexer)</li>
%% </ul>
%%
%% @param What Description of what is being validated (e.g., "Token count")
%% @param Actual The actual size/count encountered
%% @param Max The maximum allowed size/count
%% @returns `ok' if within limits, `{error, {size_exceeded, ...}}' otherwise
%%
%% @see format_size_error/3
%%
%% @example
%% ```
%% %% Within limits
%% ok = validate_size("Token count", 100000, 500000).
%%%
%% %% Exceeds limit
%% {error, {size_exceeded, "Token count", 600000, 500000}} =
%%     validate_size("Token count", 600000, 500000).
%% '''
-spec validate_size(string(), integer(), integer()) -> ok | {error, tuple()}.
validate_size(What, Actual, Max) when Actual > Max ->
    {error, {size_exceeded, What, Actual, Max}};
validate_size(_What, _Actual, _Max) ->
    ok.

%% @doc Validate depth against maximum limit.
%%
%% Checks if an actual nesting depth exceeds the maximum allowed.
%% Returns `ok' if within limits, or an error tuple if exceeded.
%%
%% The error tuple format is: `{error, {depth_exceeded, What, Actual, Max}}'
%%
%% Common uses:
%% <ul>
%%%   <li>AST depth validation (parser)</li>
%%%   <li>Pattern depth validation (parser)</li>
%%%   <li>Type depth validation (parser)</li>
%%%   <li>Lexical nesting validation (lexer)</li>
%% </ul>
%%
%% @param What Description of what is being validated (e.g., "AST depth")
%% @param Actual The actual depth encountered
%% @param Max The maximum allowed depth
%% @returns `ok' if within limits, `{error, {depth_exceeded, ...}}' otherwise
%%
%% @see format_depth_error/3
%% @see ast_depth/1
%% @see pattern_depth/1
%% @see type_depth/1
%%
%% @example
%% ```
%% %% Within limits
%% ok = validate_depth("AST depth", 250, 500).
%%%
%% %% Exceeds limit
%% {error, {depth_exceeded, "AST depth", 550, 500}} =
%%     validate_depth("AST depth", 550, 500).
%% '''
-spec validate_depth(string(), integer(), integer()) -> ok | {error, tuple()}.
validate_depth(What, Actual, Max) when Actual > Max ->
    {error, {depth_exceeded, What, Actual, Max}};
validate_depth(_What, _Actual, _Max) ->
    ok.

%% @doc Validate elapsed time against timeout limit.
%%
%% Checks if an elapsed time exceeds the maximum allowed.
%% Returns `ok' if within limits, or an error tuple if exceeded.
%%
%% The error tuple format is: `{error, {timeout, What, Elapsed, Max}}'
%%
%% Common uses:
%% <ul>
%%%   <li>Parse timeout validation (parser)</li>
%%%   <li>Compilation timeout validation (compiler)</li>
%%%   <li>Any time-bounded operation</li>
%% </ul>
%%
%% @param What Description of what is being validated (e.g., "Parse")
%% @param Elapsed The actual time elapsed in milliseconds
%% @param Max The maximum allowed time in milliseconds
%% @returns `ok' if within limits, `{error, {timeout, ...}}' otherwise
%%
%% @see format_timeout_error/3
%%
%% @example
%% ```
%% %% Within limits
%% ok = validate_timeout("Parse", 15000, 30000).
%%%
%% %% Exceeds limit
%% {error, {timeout, "Parse", 45000, 30000}} =
%%     validate_timeout("Parse", 45000, 30000).
%% '''
-spec validate_timeout(string(), integer(), integer()) -> ok | {error, tuple()}.
validate_timeout(What, Elapsed, Max) when Elapsed > Max ->
    {error, {timeout, What, Elapsed, Max}};
validate_timeout(_What, _Elapsed, _Max) ->
    ok.
