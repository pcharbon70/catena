# Feature Planning: AST Construction (Task 1.1.3)

**Date**: 2025-11-09
**Phase**: Phase 1 - Core Language Infrastructure
**Section**: 1.1 - Lexer and Parser
**Task**: 1.1.3 - AST Construction
**Status**: Planning

## 1. Problem Statement

### Current State

Tasks 1.1.1 (Lexer) and 1.1.2 (Parser) have been successfully implemented with comprehensive effect keywords support. The current implementation includes:

**AST Node Definitions** (`src/compiler/parser/catena_ast.hrl`):
- Comprehensive record definitions for all AST node types (module structure, declarations, expressions, patterns, type expressions)
- All nodes include location metadata for error reporting
- Support for algebraic effects (effect_decl, perform_expr, try_with_expr, handler_clause, operation_case)
- 40+ distinct AST node types covering the full Catena syntax

**Parser Implementation** (`src/compiler/parser/catena_parser.yrl`):
- AST nodes are constructed **inline** within parser grammar rules using tuple syntax
- Direct construction: `{shape_decl, extract_atom('$2'), '$3', '$5', [], extract_location('$1')}`
- Helper functions exist only for token extraction (`extract_atom/1`, `extract_value/1`, `extract_location/1`)
- No dedicated AST construction module or builder functions

**Existing Utilities** (`src/compiler/catena_compiler_utils.erl`):
- AST analysis functions: `ast_depth/1`, `ast_node_count/1`, `pattern_depth/1`, `type_depth/1`
- Generic traversal: `ast_map/2`, `ast_fold/3` (partially implemented)
- Token/AST extraction helpers
- Validation utilities for size/depth/timeout limits

**Parse Module** (`src/compiler/parser/catena_parse.erl`):
- High-level wrapper combining lexing and parsing
- Resource limit validation (AST depth, node count, pattern depth, type depth)
- Error formatting and handling

### Desired State

Task 1.1.3 aims to implement "AST construction functions that build structured trees from parse results." Based on analysis of the codebase, we need:

1. **Dedicated AST construction module** (`catena_ast.erl`) providing:
   - Smart constructors for all AST node types with validation
   - Location inference and tracking helpers
   - Builder pattern functions for complex nodes
   - Type-safe construction ensuring well-formed ASTs

2. **Enhanced traversal and transformation utilities**:
   - Complete `ast_map/2` implementation handling all node types
   - Complete `ast_fold/3` implementation for all node types
   - Visitor pattern support for AST analysis
   - Zipper-based navigation for local transformations

3. **AST validation framework**:
   - Well-formedness checking (e.g., pattern exhaustiveness, duplicate names)
   - Structural invariants (e.g., proper nesting, valid references)
   - Semantic pre-checks before type checking

4. **Pretty-printing and debugging**:
   - Human-readable AST output for debugging
   - S-expression or tree format for inspection
   - Diff utilities for AST comparison

5. **Testing infrastructure**:
   - QuickCheck generators for random AST generation
   - AST equivalence testing utilities
   - Round-trip testing (parse → AST → pretty-print → parse)

### Gap Analysis

| Component | Current Status | Required | Priority |
|-----------|---------------|----------|----------|
| AST node records | ✅ Complete | - | - |
| Inline construction in parser | ✅ Working | - | - |
| Smart constructors | ❌ Missing | Yes | High |
| Builder functions | ❌ Missing | Yes | Medium |
| Complete ast_map/ast_fold | ⚠️ Partial | Yes | High |
| Visitor pattern | ❌ Missing | Yes | Medium |
| Zipper navigation | ❌ Missing | Optional | Low |
| Validation framework | ⚠️ Basic | Yes | High |
| Pretty-printing | ❌ Missing | Yes | High |
| AST generators | ❌ Missing | Yes | Medium |

## 2. Solution Overview

We will implement a comprehensive AST construction and manipulation framework in three layers:

### Layer 1: Construction (catena_ast.erl)

A new module providing smart constructors that:
- **Validate inputs** (e.g., non-empty lists, valid atoms)
- **Infer locations** when possible (span from children)
- **Provide defaults** for optional fields
- **Ensure type safety** through pattern matching
- **Expose builder pattern** for complex nodes

Example:
```erlang
%% Instead of: {literal, 42, integer, {location, 1, 0}}
Lit = catena_ast:literal(42, integer, {location, 1, 0}),

%% Builder pattern for complex nodes
Flow = catena_ast:flow_decl(map)
    |> catena_ast:with_type_sig(TypeExpr)
    |> catena_ast:add_clause(Patterns1, Body1)
    |> catena_ast:add_clause(Patterns2, Body2)
    |> catena_ast:at_location(Loc).
```

### Layer 2: Traversal (catena_ast_traverse.erl)

Complete implementation of tree traversal patterns:
- **ast_map/2**: Top-down transformation (already exists, needs completion)
- **ast_fold/3**: Bottom-up accumulation (already exists, needs completion)
- **ast_fold_up/3**: Post-order fold (children before parent)
- **ast_walk/2**: Side-effect iteration (for printing, validation)
- **ast_visitor/2**: Visitor pattern with enter/exit callbacks
- **ast_transform/2**: Conditional rewriting with strategies

### Layer 3: Utilities (catena_ast_utils.erl)

High-level utilities for common AST operations:
- **Validation**: `validate/1`, `check_well_formed/1`
- **Pretty-printing**: `to_string/1`, `to_sexp/1`, `format/1`
- **Queries**: `find_all_vars/1`, `find_flow_by_name/2`, `collect_effects/1`
- **Analysis**: `free_variables/1`, `bound_variables/1`, `variable_references/2`
- **Comparison**: `ast_equal/2`, `ast_diff/2`

## 3. Technical Details

### 3.1 Smart Constructors Design

Each AST node type gets a constructor function with:
1. **Required parameters** (name, type-specific fields)
2. **Optional parameters** (location, metadata)
3. **Validation** (non-empty lists, valid types)
4. **Location inference** (from children if not provided)

**Example: Literal Constructor**
```erlang
-module(catena_ast).

%% @doc Construct a literal expression node.
%% Validates value matches type and infers location if omitted.
-spec literal(term(), atom(), location() | undefined) -> #literal{}.
literal(Value, Type, Location) when is_integer(Value), Type =:= integer ->
    #literal{value = Value, type = Type, location = ensure_location(Location)};
literal(Value, Type, Location) when is_float(Value), Type =:= float ->
    #literal{value = Value, type = Type, location = ensure_location(Location)};
literal(Value, Type, Location) when is_binary(Value), Type =:= string ->
    #literal{value = Value, type = Type, location = ensure_location(Location)};
literal(Value, Type, _) ->
    error({invalid_literal, Value, Type}).

%% @doc Helper to ensure location is present
-spec ensure_location(location() | undefined) -> location().
ensure_location(undefined) -> catena_location:new(0, 0);
ensure_location(Loc) -> Loc.
```

**Example: Binary Operation Constructor**
```erlang
%% @doc Construct a binary operation expression.
%% Infers location from operand span if not provided.
-spec binary_op(atom(), expr(), expr(), location() | undefined) -> #binary_op{}.
binary_op(Op, Left, Right, undefined) ->
    %% Infer location from left to right span
    LeftLoc = catena_compiler_utils:extract_location(Left),
    RightLoc = catena_compiler_utils:extract_location(Right),
    Loc = catena_location:span(LeftLoc, RightLoc),
    #binary_op{op = Op, left = Left, right = Right, location = Loc};
binary_op(Op, Left, Right, Location) ->
    #binary_op{op = Op, left = Left, right = Right, location = Location}.
```

### 3.2 Builder Pattern for Complex Nodes

For nodes with many optional fields (flow_decl, shape_decl), provide a builder pattern:

```erlang
%% @doc Create a flow declaration builder.
-spec flow_decl(atom()) -> flow_builder().
flow_decl(Name) ->
    #flow_builder{
        name = Name,
        type_sig = undefined,
        clauses = [],
        location = undefined
    }.

%% @doc Add type signature to flow builder.
-spec with_type_sig(flow_builder(), type_expr()) -> flow_builder().
with_type_sig(Builder, TypeSig) ->
    Builder#flow_builder{type_sig = TypeSig}.

%% @doc Add a clause to flow builder.
-spec add_clause(flow_builder(), [pattern()], expr()) -> flow_builder().
add_clause(#flow_builder{clauses = Clauses} = Builder, Patterns, Body) ->
    Clause = #flow_clause{
        patterns = Patterns,
        guards = undefined,
        body = Body,
        location = undefined
    },
    Builder#flow_builder{clauses = Clauses ++ [Clause]}.

%% @doc Build final flow_decl record.
-spec build(flow_builder()) -> #flow_decl{}.
build(#flow_builder{name = Name, type_sig = Type, clauses = Clauses, location = Loc}) ->
    #flow_decl{
        name = Name,
        type_sig = Type,
        clauses = Clauses,
        location = ensure_location(Loc)
    }.
```

### 3.3 Complete Traversal Implementation

Extend `catena_compiler_utils` or create new `catena_ast_traverse` module:

**Complete ast_fold/3 for all node types:**
```erlang
ast_fold_children(Fun, Acc, {if_expr, Cond, Then, Else, _}) ->
    Acc1 = ast_fold(Fun, Acc, Cond),
    Acc2 = ast_fold(Fun, Acc1, Then),
    ast_fold(Fun, Acc2, Else);

ast_fold_children(Fun, Acc, {let_expr, Bindings, Body, _}) ->
    Acc1 = lists:foldl(
        fun({Pat, Expr}, A) ->
            A1 = ast_fold(Fun, A, Pat),
            ast_fold(Fun, A1, Expr)
        end,
        Acc,
        Bindings
    ),
    ast_fold(Fun, Acc1, Body);

ast_fold_children(Fun, Acc, {match_expr, Clauses, _}) ->
    lists:foldl(fun(C, A) -> ast_fold(Fun, A, C) end, Acc, Clauses);

ast_fold_children(Fun, Acc, {match_clause, Pattern, Guards, Body, _}) ->
    Acc1 = ast_fold(Fun, Acc, Pattern),
    Acc2 = case Guards of
        undefined -> Acc1;
        _ -> lists:foldl(fun(G, A) -> ast_fold(Fun, A, G) end, Acc1, Guards)
    end,
    ast_fold(Fun, Acc2, Body);

%% Effect-related nodes
ast_fold_children(Fun, Acc, {perform_expr, _Effect, _Op, Args, _}) ->
    lists:foldl(fun(A, Acc0) -> ast_fold(Fun, Acc0, A) end, Acc, Args);

ast_fold_children(Fun, Acc, {try_with_expr, Body, Handlers, _}) ->
    Acc1 = ast_fold(Fun, Acc, Body),
    lists:foldl(fun(H, A) -> ast_fold(Fun, A, H) end, Acc1, Handlers);

ast_fold_children(Fun, Acc, {handler_clause, _Effect, Operations, _}) ->
    lists:foldl(fun(Op, A) -> ast_fold(Fun, A, Op) end, Acc, Operations);

ast_fold_children(Fun, Acc, {operation_case, _Op, Params, Body, _}) ->
    Acc1 = lists:foldl(fun(P, A) -> ast_fold(Fun, A, P) end, Acc, Params),
    ast_fold(Fun, Acc1, Body);

%% Record-related nodes
ast_fold_children(Fun, Acc, {record_expr, Fields, Base, _}) ->
    Acc1 = lists:foldl(
        fun({_, Expr}, A) -> ast_fold(Fun, A, Expr) end,
        Acc,
        Fields
    ),
    case Base of
        undefined -> Acc1;
        _ -> ast_fold(Fun, Acc1, Base)
    end;

ast_fold_children(Fun, Acc, {record_access, Expr, _Field, _}) ->
    ast_fold(Fun, Acc, Expr);

ast_fold_children(Fun, Acc, {pat_record, Fields, _}) ->
    lists:foldl(
        fun({_, Pat}, A) -> ast_fold(Fun, A, Pat) end,
        Acc,
        Fields
    );

%% Type-related nodes
ast_fold_children(Fun, Acc, {type_forall, _Vars, Type, _}) ->
    ast_fold(Fun, Acc, Type);

ast_fold_children(Fun, Acc, {type_record, Fields, _Extension, _}) ->
    lists:foldl(
        fun({_, Type}, A) -> ast_fold(Fun, A, Type) end,
        Acc,
        Fields
    );

ast_fold_children(Fun, Acc, {type_tuple, Elements, _}) ->
    lists:foldl(fun(T, A) -> ast_fold(Fun, A, T) end, Acc, Elements);

%% Effect type nodes
ast_fold_children(Fun, Acc, {type_effect, Type, _Effects, _}) ->
    ast_fold(Fun, Acc, Type);

ast_fold_children(Fun, Acc, {effect_decl, _Name, Operations, _}) ->
    lists:foldl(fun(Op, A) -> ast_fold(Fun, A, Op) end, Acc, Operations);

ast_fold_children(_Fun, Acc, _Other) ->
    %% Leaf nodes (literals, variables, etc.)
    Acc.
```

**Add visitor pattern support:**
```erlang
%% @doc Visit AST with enter/exit callbacks.
-spec ast_visit(Visitor, AST) -> {ok, NewAST, Acc} when
    Visitor :: #{
        enter => fun((Node, Acc) -> {continue | skip, Acc}),
        exit => fun((Node, Acc) -> Acc)
    },
    AST :: tuple(),
    Acc :: term().

ast_visit(#{enter := Enter, exit := Exit} = Visitor, AST, Acc) ->
    case Enter(AST, Acc) of
        {skip, Acc1} ->
            {AST, Acc1};
        {continue, Acc1} ->
            {AST2, Acc2} = ast_visit_children(Visitor, AST, Acc1),
            Acc3 = Exit(AST2, Acc2),
            {AST2, Acc3}
    end.
```

### 3.4 Validation Framework

Implement well-formedness checks:

```erlang
-module(catena_ast_validate).

%% @doc Validate AST is well-formed.
-spec validate(tuple()) -> ok | {error, [validation_error()]}.
validate(AST) ->
    Errors = [],
    Errors1 = check_duplicate_names(AST, Errors),
    Errors2 = check_valid_references(AST, Errors1),
    Errors3 = check_pattern_exhaustiveness(AST, Errors2),
    case Errors3 of
        [] -> ok;
        _ -> {error, lists:reverse(Errors3)}
    end.

%% @doc Check for duplicate function/type/constructor names.
check_duplicate_names({module, _, _, _, Decls, _}, Errors) ->
    Names = collect_declaration_names(Decls),
    Duplicates = find_duplicates(Names),
    case Duplicates of
        [] -> Errors;
        _ -> [{duplicate_names, Duplicates} | Errors]
    end;
check_duplicate_names(_, Errors) ->
    Errors.

%% @doc Collect all declaration names with locations.
collect_declaration_names(Decls) ->
    lists:flatmap(
        fun({shape_decl, Name, _, _, _, Loc}) -> [{shape, Name, Loc}];
           ({flow_decl, Name, _, _, Loc}) -> [{flow, Name, Loc}];
           ({effect_decl, Name, _, Loc}) -> [{effect, Name, Loc}];
           (_) -> []
        end,
        Decls
    ).

%% @doc Find duplicate names in list.
find_duplicates(Names) ->
    Grouped = lists:foldl(
        fun({Kind, Name, Loc}, Acc) ->
            Key = {Kind, Name},
            maps:update_with(Key, fun(Locs) -> [Loc | Locs] end, [Loc], Acc)
        end,
        #{},
        Names
    ),
    maps:fold(
        fun({Kind, Name}, Locs, Acc) ->
            case length(Locs) of
                N when N > 1 -> [{Kind, Name, lists:reverse(Locs)} | Acc];
                _ -> Acc
            end
        end,
        [],
        Grouped
    ).
```

### 3.5 Pretty-Printing

Implement human-readable output:

```erlang
-module(catena_ast_format).

%% @doc Convert AST to pretty-printed string.
-spec to_string(tuple()) -> string().
to_string(AST) ->
    to_string(AST, 0).

to_string({module, _, _, _, Decls, _}, Indent) ->
    DeclStrs = [to_string(D, Indent) || D <- Decls],
    string:join(DeclStrs, "\n\n");

to_string({shape_decl, Name, Params, Constructors, _, _}, Indent) ->
    Ind = indent(Indent),
    ParamStr = format_type_params(Params),
    ConstructorStr = format_constructors(Constructors, Indent + 2),
    io_lib:format("~sshape ~s~s = ~s", [Ind, Name, ParamStr, ConstructorStr]);

to_string({flow_decl, Name, Type, Clauses, _}, Indent) ->
    Ind = indent(Indent),
    TypeStr = case Type of
        undefined -> "";
        _ -> io_lib:format(" : ~s", [type_to_string(Type)])
    end,
    ClauseStrs = [format_flow_clause(C, Indent + 2) || C <- Clauses],
    io_lib:format("~sflow ~s~s\n~s", [Ind, Name, TypeStr, string:join(ClauseStrs, "\n")]);

to_string({literal, Value, Type, _}, Indent) ->
    Ind = indent(Indent),
    ValueStr = format_literal(Value, Type),
    io_lib:format("~s~s", [Ind, ValueStr]);

to_string({binary_op, Op, Left, Right, _}, Indent) ->
    LeftStr = to_string(Left, Indent),
    RightStr = to_string(Right, Indent),
    OpStr = atom_to_list(Op),
    io_lib:format("(~s ~s ~s)", [LeftStr, OpStr, RightStr]);

%% Effect nodes
to_string({perform_expr, Effect, Operation, Args, _}, Indent) ->
    Ind = indent(Indent),
    ArgStrs = [to_string(A, 0) || A <- Args],
    ArgsStr = string:join(ArgStrs, ", "),
    io_lib:format("~sperform ~s.~s(~s)", [Ind, Effect, Operation, ArgsStr]);

to_string({try_with_expr, Body, Handlers, _}, Indent) ->
    Ind = indent(Indent),
    BodyStr = to_string(Body, Indent + 2),
    HandlerStrs = [format_handler(H, Indent + 2) || H <- Handlers],
    io_lib:format("~stry\n~s\n~swith\n~s\n~send",
        [Ind, BodyStr, Ind, string:join(HandlerStrs, "\n"), Ind]);

%% Add more cases for all node types...

to_string(Node, Indent) ->
    %% Fallback: tuple format
    Ind = indent(Indent),
    io_lib:format("~s~p", [Ind, Node]).

indent(N) -> lists:duplicate(N, " ").
```

### 3.6 Testing Infrastructure

QuickCheck generators for property-based testing:

```erlang
-module(catena_ast_gen).
-include_lib("proper/include/proper.hrl").

%% @doc Generate random valid AST.
-spec gen_ast() -> proper_types:type().
gen_ast() ->
    proper_types:oneof([
        gen_literal(),
        gen_var(),
        gen_binary_op(),
        gen_if_expr(),
        gen_let_expr()
    ]).

gen_literal() ->
    ?LET({Value, Type}, gen_literal_value(),
         catena_ast:literal(Value, Type, gen_location())).

gen_literal_value() ->
    proper_types:oneof([
        {proper_types:integer(), integer},
        {proper_types:float(), float},
        {proper_types:binary(), string}
    ]).

gen_var() ->
    ?LET(Name, gen_identifier(),
         catena_ast:var(Name, gen_location())).

gen_binary_op() ->
    ?LET({Op, Left, Right}, {gen_operator(), gen_ast(), gen_ast()},
         catena_ast:binary_op(Op, Left, Right, undefined)).

gen_location() ->
    ?LET({Line, Col}, {proper_types:pos_integer(), proper_types:non_neg_integer()},
         catena_location:new(Line, Col)).

%% Property: Round-trip parsing
prop_parse_roundtrip() ->
    ?FORALL(AST, gen_ast(),
        begin
            Str = catena_ast_format:to_string(AST),
            {ok, AST2} = catena_parse:parse(Str),
            catena_ast_utils:ast_equal(AST, AST2)
        end).
```

## 4. Success Criteria

Task 1.1.3 is complete when:

1. **Smart Constructors**: All AST node types have constructor functions with validation
2. **Builder Pattern**: Complex nodes (flow_decl, shape_decl, effect_decl) have builder APIs
3. **Complete Traversal**: `ast_map/2` and `ast_fold/3` handle all 40+ node types
4. **Validation**: Well-formedness checks for common errors (duplicates, invalid refs)
5. **Pretty-Printing**: AST nodes can be formatted for debugging and inspection
6. **Testing**: Property-based tests for AST generation and validation
7. **Documentation**: All public functions have edoc documentation
8. **Integration**: Parser can optionally use smart constructors (backward compatible)

**Testing Requirements:**
- Unit tests for each smart constructor (valid and invalid inputs)
- Unit tests for traversal functions (map, fold, visitor)
- Unit tests for validation functions (detecting errors)
- Unit tests for pretty-printing (all node types)
- Property tests for round-trip parsing
- Property tests for AST equivalence
- Integration tests with existing parser

**Performance Requirements:**
- Smart constructors add < 5% overhead vs direct tuple construction
- Traversal functions handle 100k+ node ASTs without stack overflow
- Pretty-printing uses tail recursion or constant stack space

## 5. Implementation Plan

### Phase 1: Foundation (Week 1)

**Tasks:**
1. Create `catena_ast.erl` module with smart constructors for:
   - Literal expressions (`literal/3`)
   - Variable references (`var/2`)
   - Binary operations (`binary_op/4`)
   - Function application (`app/3`)
   - Basic patterns (`pat_var/2`, `pat_literal/3`, `pat_wildcard/1`)

2. Add location inference helpers:
   - `ensure_location/1` (default if undefined)
   - `infer_location/1` (span from children)
   - `merge_locations/2` (combine two locations)

3. Unit tests for constructors:
   - Valid construction
   - Invalid inputs (type mismatches)
   - Location inference

**Deliverable**: Basic smart constructors working, tests passing

### Phase 2: Complete Constructors (Week 2)

**Tasks:**
1. Add remaining expression constructors:
   - `if_expr/4`, `let_expr/3`, `match_expr/2`
   - `tuple_expr/2`, `list_expr/2`, `record_expr/3`
   - `perform_expr/4`, `try_with_expr/3`

2. Add pattern constructors:
   - `pat_constructor/3`, `pat_tuple/2`, `pat_list/2`, `pat_record/2`
   - `pat_as/3`

3. Add type expression constructors:
   - `type_fun/3`, `type_app/3`, `type_forall/3`
   - `type_tuple/2`, `type_record/3`, `type_effect/3`

4. Add declaration constructors:
   - `shape_decl/5`, `flow_decl/4`, `effect_decl/3`
   - `constructor/3`, `flow_clause/4`, `effect_operation/3`

**Deliverable**: All node types have smart constructors

### Phase 3: Builder Pattern (Week 2-3)

**Tasks:**
1. Implement builder for `flow_decl`:
   - `flow_decl/1` (name only)
   - `with_type_sig/2`, `add_clause/3`, `add_guard_clause/4`
   - `at_location/2`, `build/1`

2. Implement builder for `shape_decl`:
   - `shape_decl/1` (name only)
   - `with_type_params/2`, `add_constructor/2`, `add_derives/2`
   - `at_location/2`, `build/1`

3. Implement builder for `effect_decl`:
   - `effect_decl/1` (name only)
   - `add_operation/3`, `at_location/2`, `build/1`

4. Integration tests: Build complex ASTs using builders

**Deliverable**: Builder pattern working for major declarations

### Phase 4: Complete Traversal (Week 3)

**Tasks:**
1. Complete `ast_fold/3` in `catena_compiler_utils.erl`:
   - Add missing expression types (if, let, match, perform, try_with)
   - Add missing pattern types (pat_as, pat_record)
   - Add missing type types (type_effect, type_record)
   - Add effect declaration types

2. Complete `ast_map/2` similarly

3. Add new traversal functions:
   - `ast_fold_up/3` (post-order)
   - `ast_walk/2` (side effects only)
   - `ast_collect/2` (gather matching nodes)

4. Unit tests for all traversal modes

**Deliverable**: Complete traversal working for all node types

### Phase 5: Validation (Week 4)

**Tasks:**
1. Create `catena_ast_validate.erl` module with:
   - `validate/1` (main entry point)
   - `check_duplicate_names/2`
   - `check_valid_arities/2`
   - `check_constructor_fields/2`

2. Implement duplicate detection:
   - Flow names (same module)
   - Shape names (same module)
   - Constructor names (same shape)
   - Effect names (same module)

3. Implement basic semantic checks:
   - Flow clauses have same arity
   - Constructor field counts match uses
   - Effect operations are unique

4. Error reporting with locations

**Deliverable**: Validation framework catching common errors

### Phase 6: Pretty-Printing (Week 4)

**Tasks:**
1. Create `catena_ast_format.erl` module:
   - `to_string/1` (pretty-print)
   - `to_sexp/1` (S-expression format)
   - `to_tree/1` (tree diagram)

2. Implement formatting for all node types:
   - Proper indentation
   - Syntax highlighting (ANSI colors)
   - Configurable verbosity

3. Add diff utilities:
   - `ast_diff/2` (show differences)
   - `ast_equal/2` (structural equality)

**Deliverable**: Pretty-printing for debugging and inspection

### Phase 7: Testing & Documentation (Week 5)

**Tasks:**
1. Property-based testing with PropEr:
   - AST generators for all node types
   - Round-trip property (AST → string → AST)
   - Validation properties

2. Complete edoc documentation:
   - All public functions
   - Module overviews
   - Usage examples

3. Integration with parser:
   - Optional: Refactor parser to use smart constructors
   - Ensure backward compatibility

4. Performance benchmarks:
   - Constructor overhead
   - Traversal performance
   - Memory usage

**Deliverable**: Fully tested and documented AST construction framework

## 6. Dependencies and Risks

### Dependencies

- **catena_ast.hrl**: Must remain stable (used by parser)
- **catena_location.erl**: Required for location handling
- **catena_compiler_utils.erl**: Extends existing utilities
- **Parser (catena_parser.yrl)**: Must continue working unchanged

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Performance overhead from smart constructors | Medium | Inline hints, benchmarking, optional usage |
| Breaking changes to AST structure | High | Maintain backward compatibility, version AST |
| Incomplete traversal coverage | Medium | Comprehensive tests, pattern exhaustiveness |
| Memory usage from builders | Low | Document builder pattern as optional |

### Backward Compatibility

- Parser continues using direct tuple construction (no breaking changes)
- Smart constructors are opt-in (new API, doesn't replace old)
- AST record format unchanged (only add helpers)
- Location format stays compatible (use catena_location)

## 7. Future Enhancements (Post-PoC)

These features are valuable but deferred to later phases:

1. **Zipper Navigation**: Functional zipper for local AST updates
2. **AST Optimization**: Dead code elimination, constant folding
3. **AST Normalization**: Canonical forms for comparison
4. **AST Serialization**: Binary format for fast loading
5. **AST Versioning**: Handle multiple AST versions
6. **Interactive AST Editor**: REPL-based tree manipulation
7. **AST Visualization**: GraphViz output, web UI
8. **Incremental Parsing**: Update AST for small changes

## 8. References

**Related Code:**
- `/home/ducky/code/catena/src/compiler/parser/catena_ast.hrl` - AST record definitions
- `/home/ducky/code/catena/src/compiler/parser/catena_parser.yrl` - Parser grammar
- `/home/ducky/code/catena/src/compiler/catena_compiler_utils.erl` - Existing utilities
- `/home/ducky/code/catena/src/compiler/parser/catena_parse.erl` - High-level parser
- `/home/ducky/code/catena/src/compiler/parser/catena_location.erl` - Location tracking

**Documentation:**
- `/home/ducky/code/catena/notes/planning/proof-of-concept/phase-01.md` - Phase 1 plan
- `/home/ducky/code/catena/CLAUDE.md` - Project conventions

**Similar Implementations:**
- Erlang compiler: `erl_syntax` module (smart constructors)
- OCaml: `Pprintast` module (pretty-printing)
- Haskell GHC: `HsSyn` AST with smart constructors
- Elixir: `Macro.to_string/1` (AST formatting)
