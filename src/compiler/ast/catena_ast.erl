-module(catena_ast).

%% Smart constructors for AST nodes
-export([
    % Literals
    literal/3, literal/2,
    % Variables
    var/2,
    % Binary operations
    binary_op/4, binary_op/3,
    % Unary operations
    unary_op/3, unary_op/2,
    % Function application
    app/3, app/2,
    % Lambda expressions
    lambda/3, lambda/2,
    % Let expressions
    let_expr/3, let_expr/2,
    % If expressions
    if_expr/4, if_expr/3,
    % Match expressions
    match_expr/2, match_expr/1,
    match_clause/4, match_clause/3,
    % Lists and tuples
    list_expr/2, list_expr/1,
    tuple_expr/2, tuple_expr/1,
    % Records
    record_expr/3, record_expr/2,
    record_access/3, record_access/2,
    % Effect expressions
    perform_expr/4, perform_expr/3,
    try_with_expr/3, try_with_expr/2,
    handler_clause/3, handler_clause/2,
    operation_case/4, operation_case/3,
    % Patterns
    pat_var/2, pat_var/1,
    pat_wildcard/1, pat_wildcard/0,
    pat_constructor/3, pat_constructor/2,
    pat_literal/3, pat_literal/2,
    pat_list/2, pat_list/1,
    pat_tuple/2, pat_tuple/1,
    pat_record/2, pat_record/1,
    pat_as/3, pat_as/2,
    % Type expressions
    type_var/2, type_var/1,
    type_con/2, type_con/1,
    type_app/3, type_app/2,
    type_fun/3, type_fun/2,
    type_record/3, type_record/2,
    type_forall/3, type_forall/2,
    type_tuple/2, type_tuple/1,
    type_effect/3, type_effect/2,
    % Declarations
    shape_decl/5, shape_decl/4,
    constructor/3, constructor/2,
    flow_decl/4, flow_decl/3,
    flow_clause/4, flow_clause/3,
    effect_decl/3, effect_decl/2,
    effect_operation/3, effect_operation/2,
    trait_decl/4, trait_decl/3
]).

%% Location helpers
-export([
    location/1,
    infer_location/1,
    merge_locations/2
]).

%% Include AST record definitions
-include("../parser/catena_ast.hrl").

%%====================================================================
%% Smart Constructors - Literals
%%====================================================================

%% @doc Create a literal node with explicit location
-spec literal(integer | float | string, term(), location()) -> #literal{}.
literal(Type, Value, Loc) ->
    #literal{type = Type, value = Value, location = Loc}.

%% @doc Create a literal node with location inferred from line number
-spec literal(integer | float | string, term()) -> #literal{}.
literal(Type, Value) when is_integer(Value) ->
    literal(Type, Value, default_location());
literal(Type, Value) ->
    literal(Type, Value, default_location()).

%%====================================================================
%% Smart Constructors - Variables
%%====================================================================

%% @doc Create a variable reference with explicit location
-spec var(atom(), location()) -> #var{}.
var(Name, Loc) when is_atom(Name) ->
    #var{name = Name, location = Loc}.

%% @doc Create a variable reference with default location
-spec var(atom()) -> #var{}.
var(Name) when is_atom(Name) ->
    var(Name, default_location()).

%%====================================================================
%% Smart Constructors - Binary Operations
%%====================================================================

%% @doc Create a binary operation with explicit location
-spec binary_op(atom(), expr(), expr(), location()) -> #binary_op{}.
binary_op(Op, Left, Right, Loc) when is_atom(Op) ->
    #binary_op{op = Op, left = Left, right = Right, location = Loc}.

%% @doc Create a binary operation with location inferred from operands
-spec binary_op(atom(), expr(), expr()) -> #binary_op{}.
binary_op(Op, Left, Right) ->
    Loc = merge_locations(location(Left), location(Right)),
    binary_op(Op, Left, Right, Loc).

%%====================================================================
%% Smart Constructors - Unary Operations
%%====================================================================

%% @doc Create a unary operation with explicit location
-spec unary_op(atom(), expr(), location()) -> #unary_op{}.
unary_op(Op, Operand, Loc) when is_atom(Op) ->
    #unary_op{op = Op, operand = Operand, location = Loc}.

%% @doc Create a unary operation with location inferred from operand
-spec unary_op(atom(), expr()) -> #unary_op{}.
unary_op(Op, Operand) ->
    unary_op(Op, Operand, location(Operand)).

%%====================================================================
%% Smart Constructors - Function Application
%%====================================================================

%% @doc Create a function application with explicit location
-spec app(expr(), [expr()], location()) -> #app{}.
app(Func, Args, Loc) when is_list(Args) ->
    #app{func = Func, args = Args, location = Loc}.

%% @doc Create a function application with location inferred from function
-spec app(expr(), [expr()]) -> #app{}.
app(Func, Args) ->
    app(Func, Args, location(Func)).

%%====================================================================
%% Smart Constructors - Lambda Expressions
%%====================================================================

%% @doc Create a lambda expression with explicit location
-spec lambda([pattern()], expr(), location()) -> #lambda{}.
lambda(Params, Body, Loc) when is_list(Params) ->
    #lambda{params = Params, body = Body, location = Loc}.

%% @doc Create a lambda expression with location inferred from body
-spec lambda([pattern()], expr()) -> #lambda{}.
lambda(Params, Body) ->
    lambda(Params, Body, location(Body)).

%%====================================================================
%% Smart Constructors - Let Expressions
%%====================================================================

%% @doc Create a let expression with explicit location
-spec let_expr([{pattern(), expr()}], expr(), location()) -> #let_expr{}.
let_expr(Bindings, Body, Loc) when is_list(Bindings) ->
    #let_expr{bindings = Bindings, body = Body, location = Loc}.

%% @doc Create a let expression with location inferred from body
-spec let_expr([{pattern(), expr()}], expr()) -> #let_expr{}.
let_expr(Bindings, Body) ->
    let_expr(Bindings, Body, location(Body)).

%%====================================================================
%% Smart Constructors - If Expressions
%%====================================================================

%% @doc Create an if expression with explicit location
-spec if_expr(expr(), expr(), expr(), location()) -> #if_expr{}.
if_expr(Condition, ThenBranch, ElseBranch, Loc) ->
    #if_expr{
        condition = Condition,
        then_branch = ThenBranch,
        else_branch = ElseBranch,
        location = Loc
    }.

%% @doc Create an if expression with location inferred from condition
-spec if_expr(expr(), expr(), expr()) -> #if_expr{}.
if_expr(Condition, ThenBranch, ElseBranch) ->
    if_expr(Condition, ThenBranch, ElseBranch, location(Condition)).

%%====================================================================
%% Smart Constructors - Match Expressions
%%====================================================================

%% @doc Create a match expression with explicit location
-spec match_expr([#match_clause{}], location()) -> #match_expr{}.
match_expr(Clauses, Loc) when is_list(Clauses) ->
    #match_expr{clauses = Clauses, location = Loc}.

%% @doc Create a match expression with location inferred from first clause
-spec match_expr([#match_clause{}]) -> #match_expr{}.
match_expr([FirstClause | _] = Clauses) ->
    match_expr(Clauses, location(FirstClause));
match_expr([]) ->
    match_expr([], default_location()).

%% @doc Create a match clause with explicit location
-spec match_clause(pattern(), [expr()] | undefined, expr(), location()) -> #match_clause{}.
match_clause(Pattern, Guards, Body, Loc) ->
    #match_clause{
        pattern = Pattern,
        guards = Guards,
        body = Body,
        location = Loc
    }.

%% @doc Create a match clause with location inferred from pattern
-spec match_clause(pattern(), [expr()] | undefined, expr()) -> #match_clause{}.
match_clause(Pattern, Guards, Body) ->
    match_clause(Pattern, Guards, Body, location(Pattern)).

%%====================================================================
%% Smart Constructors - Lists and Tuples
%%====================================================================

%% @doc Create a list expression with explicit location
-spec list_expr([expr()], location()) -> #list_expr{}.
list_expr(Elements, Loc) when is_list(Elements) ->
    #list_expr{elements = Elements, location = Loc}.

%% @doc Create a list expression with location inferred from first element
-spec list_expr([expr()]) -> #list_expr{}.
list_expr([First | _] = Elements) ->
    list_expr(Elements, location(First));
list_expr([]) ->
    list_expr([], default_location()).

%% @doc Create a tuple expression with explicit location
-spec tuple_expr([expr()], location()) -> #tuple_expr{}.
tuple_expr(Elements, Loc) when is_list(Elements) ->
    #tuple_expr{elements = Elements, location = Loc}.

%% @doc Create a tuple expression with location inferred from first element
-spec tuple_expr([expr()]) -> #tuple_expr{}.
tuple_expr([First | _] = Elements) ->
    tuple_expr(Elements, location(First));
tuple_expr([]) ->
    tuple_expr([], default_location()).

%%====================================================================
%% Smart Constructors - Records
%%====================================================================

%% @doc Create a record expression with explicit location
-spec record_expr([{atom(), expr()}], expr() | undefined, location()) -> #record_expr{}.
record_expr(Fields, Base, Loc) when is_list(Fields) ->
    #record_expr{fields = Fields, base = Base, location = Loc}.

%% @doc Create a record expression with location inferred from first field
-spec record_expr([{atom(), expr()}], expr() | undefined) -> #record_expr{}.
record_expr([{_, FirstValue} | _] = Fields, Base) ->
    record_expr(Fields, Base, location(FirstValue));
record_expr([], Base) ->
    record_expr([], Base, default_location()).

%% @doc Create a record field access with explicit location
-spec record_access(expr(), atom(), location()) -> #record_access{}.
record_access(Record, Field, Loc) when is_atom(Field) ->
    #record_access{record = Record, field = Field, location = Loc}.

%% @doc Create a record field access with location inferred from record
-spec record_access(expr(), atom()) -> #record_access{}.
record_access(Record, Field) ->
    record_access(Record, Field, location(Record)).

%%====================================================================
%% Smart Constructors - Effect Expressions
%%====================================================================

%% @doc Create a perform expression with explicit location
-spec perform_expr(atom(), atom(), [expr()], location()) -> #perform_expr{}.
perform_expr(Effect, Operation, Args, Loc) when is_atom(Effect), is_atom(Operation), is_list(Args) ->
    #perform_expr{
        effect = Effect,
        operation = Operation,
        args = Args,
        location = Loc
    }.

%% @doc Create a perform expression with default location
-spec perform_expr(atom(), atom(), [expr()]) -> #perform_expr{}.
perform_expr(Effect, Operation, Args) ->
    perform_expr(Effect, Operation, Args, default_location()).

%% @doc Create a try-with expression with explicit location
-spec try_with_expr(expr(), [#handler_clause{}], location()) -> #try_with_expr{}.
try_with_expr(Body, Handlers, Loc) when is_list(Handlers) ->
    #try_with_expr{body = Body, handlers = Handlers, location = Loc}.

%% @doc Create a try-with expression with location inferred from body
-spec try_with_expr(expr(), [#handler_clause{}]) -> #try_with_expr{}.
try_with_expr(Body, Handlers) ->
    try_with_expr(Body, Handlers, location(Body)).

%% @doc Create a handler clause with explicit location
-spec handler_clause(atom(), [#operation_case{}], location()) -> #handler_clause{}.
handler_clause(Effect, Operations, Loc) when is_atom(Effect), is_list(Operations) ->
    #handler_clause{effect = Effect, operations = Operations, location = Loc}.

%% @doc Create a handler clause with default location
-spec handler_clause(atom(), [#operation_case{}]) -> #handler_clause{}.
handler_clause(Effect, Operations) ->
    handler_clause(Effect, Operations, default_location()).

%% @doc Create an operation case with explicit location
-spec operation_case(atom(), [pattern()], expr(), location()) -> #operation_case{}.
operation_case(Operation, Params, Body, Loc) when is_atom(Operation), is_list(Params) ->
    #operation_case{
        operation = Operation,
        params = Params,
        body = Body,
        location = Loc
    }.

%% @doc Create an operation case with location inferred from body
-spec operation_case(atom(), [pattern()], expr()) -> #operation_case{}.
operation_case(Operation, Params, Body) ->
    operation_case(Operation, Params, Body, location(Body)).

%%====================================================================
%% Smart Constructors - Patterns
%%====================================================================

%% @doc Create a variable pattern with explicit location
-spec pat_var(atom(), location()) -> #pat_var{}.
pat_var(Name, Loc) when is_atom(Name) ->
    #pat_var{name = Name, location = Loc}.

%% @doc Create a variable pattern with default location
-spec pat_var(atom()) -> #pat_var{}.
pat_var(Name) ->
    pat_var(Name, default_location()).

%% @doc Create a wildcard pattern with explicit location
-spec pat_wildcard(location()) -> #pat_wildcard{}.
pat_wildcard(Loc) ->
    #pat_wildcard{location = Loc}.

%% @doc Create a wildcard pattern with default location
-spec pat_wildcard() -> #pat_wildcard{}.
pat_wildcard() ->
    pat_wildcard(default_location()).

%% @doc Create a constructor pattern with explicit location
-spec pat_constructor(atom(), [pattern()], location()) -> #pat_constructor{}.
pat_constructor(Name, Args, Loc) when is_atom(Name), is_list(Args) ->
    #pat_constructor{name = Name, args = Args, location = Loc}.

%% @doc Create a constructor pattern with default location
-spec pat_constructor(atom(), [pattern()]) -> #pat_constructor{}.
pat_constructor(Name, Args) ->
    pat_constructor(Name, Args, default_location()).

%% @doc Create a literal pattern with explicit location
-spec pat_literal(term(), integer | float | string, location()) -> #pat_literal{}.
pat_literal(Value, Type, Loc) ->
    #pat_literal{value = Value, type = Type, location = Loc}.

%% @doc Create a literal pattern with default location
-spec pat_literal(term(), integer | float | string) -> #pat_literal{}.
pat_literal(Value, Type) ->
    pat_literal(Value, Type, default_location()).

%% @doc Create a list pattern with explicit location
-spec pat_list([pattern()], location()) -> #pat_list{}.
pat_list(Elements, Loc) when is_list(Elements) ->
    #pat_list{elements = Elements, location = Loc}.

%% @doc Create a list pattern with location inferred from first element
-spec pat_list([pattern()]) -> #pat_list{}.
pat_list([First | _] = Elements) ->
    pat_list(Elements, location(First));
pat_list([]) ->
    pat_list([], default_location()).

%% @doc Create a tuple pattern with explicit location
-spec pat_tuple([pattern()], location()) -> #pat_tuple{}.
pat_tuple(Elements, Loc) when is_list(Elements) ->
    #pat_tuple{elements = Elements, location = Loc}.

%% @doc Create a tuple pattern with location inferred from first element
-spec pat_tuple([pattern()]) -> #pat_tuple{}.
pat_tuple([First | _] = Elements) ->
    pat_tuple(Elements, location(First));
pat_tuple([]) ->
    pat_tuple([], default_location()).

%% @doc Create a record pattern with explicit location
-spec pat_record([{atom(), pattern()}], location()) -> #pat_record{}.
pat_record(Fields, Loc) when is_list(Fields) ->
    #pat_record{fields = Fields, location = Loc}.

%% @doc Create a record pattern with location inferred from first field
-spec pat_record([{atom(), pattern()}]) -> #pat_record{}.
pat_record([{_, FirstPat} | _] = Fields) ->
    pat_record(Fields, location(FirstPat));
pat_record([]) ->
    pat_record([], default_location()).

%% @doc Create an as-pattern with explicit location
-spec pat_as(atom(), pattern(), location()) -> #pat_as{}.
pat_as(Name, Pattern, Loc) when is_atom(Name) ->
    #pat_as{name = Name, pattern = Pattern, location = Loc}.

%% @doc Create an as-pattern with location inferred from pattern
-spec pat_as(atom(), pattern()) -> #pat_as{}.
pat_as(Name, Pattern) ->
    pat_as(Name, Pattern, location(Pattern)).

%%====================================================================
%% Smart Constructors - Type Expressions
%%====================================================================

%% @doc Create a type variable with explicit location
-spec type_var(atom(), location()) -> #type_var{}.
type_var(Name, Loc) when is_atom(Name) ->
    #type_var{name = Name, location = Loc}.

%% @doc Create a type variable with default location
-spec type_var(atom()) -> #type_var{}.
type_var(Name) ->
    type_var(Name, default_location()).

%% @doc Create a type constructor with explicit location
-spec type_con(atom(), location()) -> #type_con{}.
type_con(Name, Loc) when is_atom(Name) ->
    #type_con{name = Name, location = Loc}.

%% @doc Create a type constructor with default location
-spec type_con(atom()) -> #type_con{}.
type_con(Name) ->
    type_con(Name, default_location()).

%% @doc Create a type application with explicit location
-spec type_app(type_expr(), [type_expr()], location()) -> #type_app{}.
type_app(Constructor, Args, Loc) when is_list(Args) ->
    #type_app{constructor = Constructor, args = Args, location = Loc}.

%% @doc Create a type application with location inferred from constructor
-spec type_app(type_expr(), [type_expr()]) -> #type_app{}.
type_app(Constructor, Args) ->
    type_app(Constructor, Args, location(Constructor)).

%% @doc Create a function type with explicit location
-spec type_fun(type_expr(), type_expr(), location()) -> #type_fun{}.
type_fun(From, To, Loc) ->
    #type_fun{from = From, to = To, location = Loc}.

%% @doc Create a function type with location inferred from parameter type
-spec type_fun(type_expr(), type_expr()) -> #type_fun{}.
type_fun(From, To) ->
    type_fun(From, To, location(From)).

%% @doc Create a record type with explicit location
-spec type_record([{atom(), type_expr()}], atom() | undefined, location()) -> #type_record{}.
type_record(Fields, RowVar, Loc) when is_list(Fields) ->
    #type_record{fields = Fields, row_var = RowVar, location = Loc}.

%% @doc Create a record type with location inferred from first field
-spec type_record([{atom(), type_expr()}], atom() | undefined) -> #type_record{}.
type_record([{_, FirstType} | _] = Fields, RowVar) ->
    type_record(Fields, RowVar, location(FirstType));
type_record([], RowVar) ->
    type_record([], RowVar, default_location()).

%% @doc Create a forall type with explicit location
-spec type_forall([atom()], type_expr(), location()) -> #type_forall{}.
type_forall(TypeVars, Type, Loc) when is_list(TypeVars) ->
    #type_forall{type_vars = TypeVars, type = Type, location = Loc}.

%% @doc Create a forall type with location inferred from type
-spec type_forall([atom()], type_expr()) -> #type_forall{}.
type_forall(TypeVars, Type) ->
    type_forall(TypeVars, Type, location(Type)).

%% @doc Create a tuple type with explicit location
-spec type_tuple([type_expr()], location()) -> #type_tuple{}.
type_tuple(Elements, Loc) when is_list(Elements) ->
    #type_tuple{elements = Elements, location = Loc}.

%% @doc Create a tuple type with location inferred from first element
-spec type_tuple([type_expr()]) -> #type_tuple{}.
type_tuple([First | _] = Elements) ->
    type_tuple(Elements, location(First));
type_tuple([]) ->
    type_tuple([], default_location()).

%% @doc Create an effect-annotated type with explicit location
-spec type_effect(type_expr(), [atom()], location()) -> #type_effect{}.
type_effect(Type, Effects, Loc) when is_list(Effects) ->
    #type_effect{type = Type, effects = Effects, location = Loc}.

%% @doc Create an effect-annotated type with location inferred from type
-spec type_effect(type_expr(), [atom()]) -> #type_effect{}.
type_effect(Type, Effects) ->
    type_effect(Type, Effects, location(Type)).

%%====================================================================
%% Smart Constructors - Declarations
%%====================================================================

%% @doc Create a shape declaration with explicit location
-spec shape_decl(atom(), [atom()], [#constructor{}], [atom()], location()) -> #shape_decl{}.
shape_decl(Name, TypeParams, Constructors, Derives, Loc) when is_atom(Name), is_list(TypeParams), is_list(Constructors), is_list(Derives) ->
    #shape_decl{
        name = Name,
        type_params = TypeParams,
        constructors = Constructors,
        derives = Derives,
        location = Loc
    }.

%% @doc Create a shape declaration with location inferred from first constructor
-spec shape_decl(atom(), [atom()], [#constructor{}], [atom()]) -> #shape_decl{}.
shape_decl(Name, TypeParams, [FirstCons | _] = Constructors, Derives) ->
    shape_decl(Name, TypeParams, Constructors, Derives, location(FirstCons));
shape_decl(Name, TypeParams, [], Derives) ->
    shape_decl(Name, TypeParams, [], Derives, default_location()).

%% @doc Create a constructor with explicit location
-spec constructor(atom(), [type_expr()], location()) -> #constructor{}.
constructor(Name, Fields, Loc) when is_atom(Name), is_list(Fields) ->
    #constructor{name = Name, fields = Fields, location = Loc}.

%% @doc Create a constructor with location inferred from first field
-spec constructor(atom(), [type_expr()]) -> #constructor{}.
constructor(Name, [FirstField | _] = Fields) ->
    constructor(Name, Fields, location(FirstField));
constructor(Name, []) ->
    constructor(Name, [], default_location()).

%% @doc Create a flow declaration with explicit location
-spec flow_decl(atom(), type_expr() | undefined, [#flow_clause{}], location()) -> #flow_decl{}.
flow_decl(Name, TypeSig, Clauses, Loc) when is_atom(Name), is_list(Clauses) ->
    #flow_decl{
        name = Name,
        type_sig = TypeSig,
        clauses = Clauses,
        location = Loc
    }.

%% @doc Create a flow declaration with location inferred from first clause
-spec flow_decl(atom(), type_expr() | undefined, [#flow_clause{}]) -> #flow_decl{}.
flow_decl(Name, TypeSig, [FirstClause | _] = Clauses) ->
    flow_decl(Name, TypeSig, Clauses, location(FirstClause));
flow_decl(Name, TypeSig, []) ->
    flow_decl(Name, TypeSig, [], default_location()).

%% @doc Create a flow clause with explicit location
-spec flow_clause([pattern()], [expr()] | undefined, expr(), location()) -> #flow_clause{}.
flow_clause(Patterns, Guards, Body, Loc) when is_list(Patterns) ->
    #flow_clause{
        patterns = Patterns,
        guards = Guards,
        body = Body,
        location = Loc
    }.

%% @doc Create a flow clause with location inferred from body
-spec flow_clause([pattern()], [expr()] | undefined, expr()) -> #flow_clause{}.
flow_clause(Patterns, Guards, Body) ->
    flow_clause(Patterns, Guards, Body, location(Body)).

%% @doc Create an effect declaration with explicit location
-spec effect_decl(atom(), [#effect_operation{}], location()) -> #effect_decl{}.
effect_decl(Name, Operations, Loc) when is_atom(Name), is_list(Operations) ->
    #effect_decl{name = Name, operations = Operations, location = Loc}.

%% @doc Create an effect declaration with location inferred from first operation
-spec effect_decl(atom(), [#effect_operation{}]) -> #effect_decl{}.
effect_decl(Name, [FirstOp | _] = Operations) ->
    effect_decl(Name, Operations, location(FirstOp));
effect_decl(Name, []) ->
    effect_decl(Name, [], default_location()).

%% @doc Create an effect operation with explicit location
-spec effect_operation(atom(), type_expr() | undefined, location()) -> #effect_operation{}.
effect_operation(Name, TypeSig, Loc) when is_atom(Name) ->
    #effect_operation{name = Name, type_sig = TypeSig, location = Loc}.

%% @doc Create an effect operation with default location
-spec effect_operation(atom(), type_expr() | undefined) -> #effect_operation{}.
effect_operation(Name, TypeSig) ->
    effect_operation(Name, TypeSig, default_location()).

%% @doc Create a trait declaration with explicit location
-spec trait_decl(atom(), [atom()], [{atom(), type_expr()}], location()) -> #trait_decl{}.
trait_decl(Name, TypeParams, Methods, Loc) when is_atom(Name), is_list(TypeParams), is_list(Methods) ->
    #trait_decl{
        name = Name,
        type_params = TypeParams,
        methods = Methods,
        location = Loc
    }.

%% @doc Create a trait declaration with default location
-spec trait_decl(atom(), [atom()], [{atom(), type_expr()}]) -> #trait_decl{}.
trait_decl(Name, TypeParams, Methods) ->
    trait_decl(Name, TypeParams, Methods, default_location()).

%%====================================================================
%% Location Helpers
%%====================================================================

%% @doc Create a default location (line 1)
default_location() ->
    {line, 1}.

%% @doc Extract location from an AST node
location({line, _} = Loc) ->
    Loc;
location({location, _, _} = Loc) ->
    Loc;
location({location, _, _, _, _} = Loc) ->
    Loc;
location(Node) when is_tuple(Node) ->
    %% Extract location from last element of tuple (AST node convention)
    case element(tuple_size(Node), Node) of
        {line, _} = Loc -> Loc;
        {location, _, _} = Loc -> Loc;
        {location, _, _, _, _} = Loc -> Loc;
        _ -> default_location()
    end;
location(_) ->
    default_location().

%% @doc Infer location from a list of nodes (uses first non-empty location)
infer_location([]) ->
    default_location();
infer_location([First | _Rest]) ->
    location(First).

%% @doc Merge two locations (returns the first location)
merge_locations(Loc1, _Loc2) ->
    Loc1.
