%% Topos AST - Abstract Syntax Tree Node Definitions
%% Phase 1, Task 1.1.2: Grammar Implementation
%%
%% This header file defines all AST node types for the Topos compiler.
%% All nodes include location metadata for error reporting.

%% Location metadata
-type location() :: {line, pos_integer()}.

%%====================================================================
%% Module Structure
%%====================================================================

-record(module, {
    name :: atom(),
    exports :: [export()],
    imports :: [import()],
    declarations :: [declaration()],
    location :: location()
}).

-record(export, {
    name :: atom(),
    arity :: non_neg_integer(),
    location :: location()
}).

-record(import, {
    module :: atom(),
    items :: [atom()] | all,
    qualified :: boolean(),
    alias :: atom() | undefined,
    location :: location()
}).

-type export() :: #export{}.
-type import() :: #import{}.

%%====================================================================
%% Declarations
%%====================================================================

%% Type declaration (algebraic data type)
-record(type_decl, {
    name :: atom(),
    type_params :: [atom()],
    constructors :: list(), % [#constructor{}] - forward ref commented temporarily
    derives :: [atom()],
    location :: location()
}).

-record(constructor, {
    name :: atom(),
    fields :: [type_expr()],
    location :: location()
}).

-type constructor() :: #constructor{}.

%% Transform declaration (function definition)
-record(transform_decl, {
    name :: atom(),
    type_sig :: type_expr() | undefined,
    clauses :: list(), % [#transform_clause{}] - forward ref commented temporarily
    location :: location()
}).

-record(transform_clause, {
    patterns :: [pattern()],
    guards :: [expr()] | undefined,
    body :: expr(),
    location :: location()
}).

-type transform_clause() :: #transform_clause{}.

%% Trait declaration (type class)
-record(trait_decl, {
    name :: atom(),
    type_params :: [atom()],
    extends :: [trait_constraint()] | undefined,
    methods :: [{atom(), type_expr()}],
    default_methods :: [{atom(), expr()}] | undefined,
    location :: location()
}).

%% Trait constraint for trait hierarchies (e.g., Applicative m in "Monad m extends Applicative m")
-record(trait_constraint, {
    trait :: atom(),
    type_args :: [type_expr()],
    location :: location()
}).

%% Instance declaration (trait implementation)
-record(instance_decl, {
    trait :: atom(),
    type_args :: [type_expr()],
    constraints :: [trait_constraint()] | undefined,
    methods :: [{atom(), expr()}],
    location :: location()
}).

%% Effect declaration (algebraic effect interface)
-record(effect_decl, {
    name :: atom(),
    operations :: list(effect_operation()), % [#effect_operation{}]
    supereffects :: [atom()], % NEW: Effect inheritance/polymorphism
    type_params :: [atom()], % NEW: Effect type parameters
    constraints :: [effect_constraint()], % NEW: Effect constraints
    location :: location()
}).

-record(effect_operation, {
    name :: atom(),
    type_sig :: type_expr() | undefined,
    location :: location()
}).

%% NEW: Effect constraint for polymorphic effects
-record(effect_constraint, {
    effect_var :: atom(), % Type variable for effect
    super_effect :: atom() | effect_expr(), % Required supereffect
    location :: location()
}).

%% NEW: Effect expression tree (for complex effect types)
-record(effect_var, {
    name :: atom(),
    location :: location()
}).

-record(effect_app, {
    effect :: atom() | effect_expr(),
    args :: [type_expr()],
    location :: location()
}).

-record(effect_intersection, {
    left :: effect_expr(),
    right :: effect_expr(),
    location :: location()
}).

-record(effect_union, {
    left :: effect_expr(),
    right :: effect_expr(),
    location :: location()
}).

%% Type definitions for polymorphic effects
-type effect_operation() :: #effect_operation{}.
-type effect_constraint() :: #effect_constraint{}.
-type effect_expr() :: atom() | #effect_var{} | #effect_app{} |
                     #effect_intersection{} | #effect_union{}.

%% Type definitions for trait system
-type trait_constraint() :: #trait_constraint{}.

-type declaration() :: #type_decl{} | #transform_decl{} | #trait_decl{} |
                       #instance_decl{} | #effect_decl{}.

%%====================================================================
%% Expressions
%%====================================================================

%% Literal values
-record(literal, {
    value :: integer() | float() | binary(),
    type :: integer | float | string,
    location :: location()
}).

%% Variable reference
-record(var, {
    name :: atom(),
    location :: location()
}).

%% Function application
-record(app, {
    func :: expr(),
    args :: [expr()],
    location :: location()
}).

%% Lambda expression
-record(lambda, {
    params :: [pattern()],
    body :: expr(),
    location :: location()
}).

%% Let binding
-record(let_expr, {
    bindings :: [{pattern(), expr()}],
    body :: expr(),
    location :: location()
}).

%% Match expression (pattern matching)
-record(match_expr, {
    clauses :: list(), % [#match_clause{}] - forward ref commented temporarily
    location :: location()
}).

-record(match_clause, {
    pattern :: pattern(),
    guards :: [expr()] | undefined,
    body :: expr(),
    location :: location()
}).

-type match_clause() :: #match_clause{}.

%% If expression
-record(if_expr, {
    condition :: expr(),
    then_branch :: expr(),
    else_branch :: expr(),
    location :: location()
}).

%% Do expression (monadic sequencing)
-record(do_expr, {
    statements :: [do_statement()],
    location :: location()
}).

-type do_statement() :: {bind, pattern(), expr()} | {expr, expr()}.

%% Binary operation
-record(binary_op, {
    op :: atom(),
    left :: expr(),
    right :: expr(),
    location :: location()
}).

%% Unary operation
-record(unary_op, {
    op :: atom(),
    operand :: expr(),
    location :: location()
}).

%% Record expression
-record(record_expr, {
    fields :: [{atom(), expr()}],
    base :: expr() | undefined,  % For record update
    location :: location()
}).

%% Record field access
-record(record_access, {
    record :: expr(),
    field :: atom(),
    location :: location()
}).

%% List expression
-record(list_expr, {
    elements :: [expr()],
    location :: location()
}).

%% Tuple expression
-record(tuple_expr, {
    elements :: [expr()],
    location :: location()
}).

%% Perform expression (invoke effect operation)
-record(perform_expr, {
    effect :: atom(),
    operation :: atom(),
    args :: [expr()],
    location :: location()
}).

%% Try-with expression (effect handler)
-record(try_with_expr, {
    body :: expr(),
    handlers :: list(), % [#handler_clause{}] - forward ref commented temporarily
    location :: location()
}).

-record(handler_clause, {
    effect :: atom(),
    operations :: list(), % [#operation_case{}] - forward ref commented temporarily
    location :: location()
}).

-type handler_clause() :: #handler_clause{}.

-record(operation_case, {
    operation :: atom(),
    params :: [pattern()],
    body :: expr(),
    location :: location()
}).

-type operation_case() :: #operation_case{}.

-type expr() :: #literal{} | #var{} | #app{} | #lambda{} | #let_expr{} |
                #match_expr{} | #if_expr{} | #do_expr{} | #record_expr{} |
                #record_access{} | #binary_op{} | #unary_op{} | #list_expr{} |
                #tuple_expr{} | #perform_expr{} | #try_with_expr{}.

%%====================================================================
%% Patterns
%%====================================================================

%% Variable pattern
-record(pat_var, {
    name :: atom(),
    location :: location()
}).

%% Constructor pattern
-record(pat_constructor, {
    name :: atom(),
    args :: [pattern()],
    location :: location()
}).

%% Record pattern
-record(pat_record, {
    fields :: [{atom(), pattern()}],
    location :: location()
}).

%% Literal pattern
-record(pat_literal, {
    value :: integer() | float() | binary(),
    type :: integer | float | string,
    location :: location()
}).

%% Wildcard pattern (_)
-record(pat_wildcard, {
    location :: location()
}).

%% As-pattern (x@pattern)
-record(pat_as, {
    name :: atom(),
    pattern :: pattern(),
    location :: location()
}).

%% List pattern
-record(pat_list, {
    elements :: [pattern()],
    location :: location()
}).

%% Tuple pattern
-record(pat_tuple, {
    elements :: [pattern()],
    location :: location()
}).

-type pattern() :: #pat_var{} | #pat_constructor{} | #pat_record{} |
                   #pat_literal{} | #pat_wildcard{} | #pat_as{} |
                   #pat_list{} | #pat_tuple{}.

%%====================================================================
%% Type Expressions
%%====================================================================

%% Type variable
-record(type_var, {
    name :: atom(),
    location :: location()
}).

%% Type constructor
-record(type_con, {
    name :: atom(),
    location :: location()
}).

%% Type application
-record(type_app, {
    constructor :: type_expr(),
    args :: [type_expr()],
    location :: location()
}).

%% Function type
-record(type_fun, {
    from :: type_expr(),
    to :: type_expr(),
    location :: location()
}).

%% Record type
-record(type_record, {
    fields :: [{atom(), type_expr()}],
    row_var :: atom() | undefined,
    location :: location()
}).

%% Forall quantification
-record(type_forall, {
    type_vars :: [atom()],
    type :: type_expr(),
    location :: location()
}).

%% Tuple type
-record(type_tuple, {
    elements :: [type_expr()],
    location :: location()
}).

%% Effect annotation (Type / {Effect1, Effect2})
%% ENHANCED: Supports effect expressions and polymorphism
-record(type_effect, {
    type :: type_expr(),
    effects :: effect_expr() | [effect_expr()], % NEW: Complex effect expressions
    location :: location()
}).

%% NEW: Effect quantification for polymorphic effects
-record(type_effect_forall, {
    effect_vars :: [atom()], % Effect type variables
    constraints :: [effect_constraint()], % Effect constraints
    body :: type_expr(), % Type with effect variables
    location :: location()
}).

%% NEW: Effect bounds in type signatures
-record(effect_bounds, {
    var :: atom(),
    lower_bounds :: [effect_expr()], % Var ≥ these effects
    upper_bounds :: [effect_expr()], % Var ≤ these effects
    location :: location()
}).

-type type_expr() :: #type_var{} | #type_con{} | #type_app{} |
                     #type_fun{} | #type_record{} | #type_forall{} |
                     #type_tuple{} | #type_effect{} | #type_effect_forall{} |
                     #effect_bounds{}.
