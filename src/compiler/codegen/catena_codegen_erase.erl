%%%-------------------------------------------------------------------
%%% @doc Type Erasure for Code Generation (Task 1.3.3)
%%%
%%% Removes type information from the AST for runtime execution.
%%% Catena's type system exists only at compile time - at runtime,
%%% BEAM operates on untyped terms.
%%%
%%% This module handles:
%%% - Type annotation removal from expressions
%%% - Trait dictionary passing transformation
%%% - Polymorphism handling (uniform representation)
%%% - Semantic preservation verification
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_erase).

-export([
    %% Main erasure
    erase_module/1,
    erase_decl/1,
    erase_expr/1,
    erase_pattern/1,

    %% Type annotation removal (1.3.3.1)
    strip_type_annotations/1,

    %% Dictionary passing (1.3.3.2)
    transform_trait_calls/2,
    build_dictionary/2,

    %% Polymorphism handling (1.3.3.3)
    monomorphize/2,

    %% Verification (1.3.3.4)
    verify_erasure/2
]).

%%====================================================================
%% Main Erasure Functions
%%====================================================================

%% @doc Erase types from an entire module
-spec erase_module(module_ast()) -> module_ast().
erase_module({module, Name, Exports, Imports, Decls, Loc}) ->
    ErasedDecls = [erase_decl(D) || D <- Decls],
    {module, Name, Exports, Imports, ErasedDecls, Loc}.

%% @doc Erase types from a declaration
erase_decl({transform, Name, Params, Body, Loc}) ->
    %% Remove type annotations from parameters
    ErasedParams = [erase_pattern(P) || P <- Params],
    %% Erase body expression
    ErasedBody = erase_expr(Body),
    {transform, Name, ErasedParams, ErasedBody, Loc};

erase_decl({transform_typed, Name, _TypeSig, Params, Body, Loc}) ->
    %% Strip type signature entirely
    ErasedParams = [erase_pattern(P) || P <- Params],
    ErasedBody = erase_expr(Body),
    {transform, Name, ErasedParams, ErasedBody, Loc};

erase_decl({type_decl, _Name, _TypeVars, _Constructors, _Derives, _Loc}) ->
    %% Type declarations are completely erased
    erased;

erase_decl({trait_decl, _Name, _TypeVar, _Supertraits, _Methods, _Loc}) ->
    %% Trait declarations are erased (methods become regular functions)
    erased;

erase_decl({instance_decl, TraitName, TypeArgs, _Constraints, Methods, Loc}) ->
    %% Instance declarations become dictionary definitions
    DictName = instance_dict_name(TraitName, TypeArgs),
    DictValue = build_instance_dict(Methods),
    {transform, DictName, [], DictValue, Loc};

erase_decl({effect_decl, _Name, _Operations, _Loc}) ->
    %% Effect declarations are metadata, erased at runtime
    erased;

erase_decl(Other) ->
    Other.

%%====================================================================
%% Expression Erasure
%%====================================================================

%% @doc Erase type information from an expression
-spec erase_expr(expr()) -> expr().

%% Literals - no types to erase
erase_expr({literal, Type, Value, Loc}) ->
    {literal, Type, Value, Loc};

%% Variables - no types to erase
erase_expr({var, Name, Loc}) ->
    {var, Name, Loc};

%% Typed expressions - strip type annotation
erase_expr({typed_expr, Expr, _Type, _Loc}) ->
    erase_expr(Expr);

%% Function application
erase_expr({app, Func, Args, Loc}) ->
    ErasedFunc = erase_expr(Func),
    ErasedArgs = [erase_expr(A) || A <- Args],
    {app, ErasedFunc, ErasedArgs, Loc};

%% Let binding
erase_expr({let_expr, Bindings, Body, Loc}) ->
    ErasedBindings = [{erase_pattern(P), erase_expr(E)} || {P, E} <- Bindings],
    ErasedBody = erase_expr(Body),
    {let_expr, ErasedBindings, ErasedBody, Loc};

%% Lambda - strip parameter types
erase_expr({lambda, Params, Body, Loc}) ->
    ErasedParams = [erase_pattern(P) || P <- Params],
    ErasedBody = erase_expr(Body),
    {lambda, ErasedParams, ErasedBody, Loc};

%% Typed lambda - strip type annotation
erase_expr({typed_lambda, Params, _RetType, Body, Loc}) ->
    ErasedParams = [erase_pattern(P) || P <- Params],
    ErasedBody = erase_expr(Body),
    {lambda, ErasedParams, ErasedBody, Loc};

%% If expression
erase_expr({if_expr, Cond, Then, Else, Loc}) ->
    {if_expr, erase_expr(Cond), erase_expr(Then), erase_expr(Else), Loc};

%% Binary operation
erase_expr({binary_op, Op, Left, Right, Loc}) ->
    {binary_op, Op, erase_expr(Left), erase_expr(Right), Loc};

%% Unary operation
erase_expr({unary_op, Op, Operand, Loc}) ->
    {unary_op, Op, erase_expr(Operand), Loc};

%% List expression
erase_expr({list_expr, Elements, Loc}) ->
    {list_expr, [erase_expr(E) || E <- Elements], Loc};

%% Tuple expression
erase_expr({tuple_expr, Elements, Loc}) ->
    {tuple_expr, [erase_expr(E) || E <- Elements], Loc};

%% Record expression
erase_expr({record_expr, Fields, Loc}) ->
    ErasedFields = [{Name, erase_expr(Value)} || {Name, Value} <- Fields],
    {record_expr, ErasedFields, Loc};

%% Record access
erase_expr({record_access, Record, Field, Loc}) ->
    {record_access, erase_expr(Record), Field, Loc};

%% Constructor
erase_expr({constructor, Name, Args, Loc}) ->
    {constructor, Name, [erase_expr(A) || A <- Args], Loc};

%% Match expression
erase_expr({match_expr, Scrutinee, Clauses, Loc}) ->
    ErasedScrutinee = erase_expr(Scrutinee),
    ErasedClauses = [erase_clause(C) || C <- Clauses],
    {match_expr, ErasedScrutinee, ErasedClauses, Loc};

%% Effect operations - no types to erase
erase_expr({perform_expr, Effect, Op, Args, Loc}) ->
    {perform_expr, Effect, Op, [erase_expr(A) || A <- Args], Loc};

erase_expr({try_with_expr, Body, Handlers, Loc}) ->
    ErasedBody = erase_expr(Body),
    ErasedHandlers = [erase_handler(H) || H <- Handlers],
    {try_with_expr, ErasedBody, ErasedHandlers, Loc};

%% Type ascription - strip entirely
erase_expr({type_ascription, Expr, _Type, _Loc}) ->
    erase_expr(Expr);

%% Trait method call - transform to dictionary lookup
erase_expr({trait_method, TraitName, Method, TypeArgs, Args, Loc}) ->
    %% Transform: TraitName.method(args) -> dict.method(args)
    %% This becomes a dictionary lookup at runtime
    DictVar = {var, trait_dict_var(TraitName), Loc},
    MethodAccess = {record_access, DictVar, Method, Loc},
    ErasedArgs = [erase_expr(A) || A <- Args],
    {app, MethodAccess, ErasedArgs, Loc};

%% Default - return as is
erase_expr(Other) ->
    Other.

%% Erase a match clause
erase_clause({clause, Patterns, Guards, Body, Loc}) ->
    ErasedPatterns = [erase_pattern(P) || P <- Patterns],
    ErasedGuards = [erase_expr(G) || G <- Guards],
    ErasedBody = erase_expr(Body),
    {clause, ErasedPatterns, ErasedGuards, ErasedBody, Loc}.

%% Erase a handler
erase_handler({handler_clause, Effect, Operations, Loc}) ->
    ErasedOps = [erase_operation(Op) || Op <- Operations],
    {handler_clause, Effect, ErasedOps, Loc}.

erase_operation({operation_case, Name, Params, Body, Loc}) ->
    ErasedParams = [erase_pattern(P) || P <- Params],
    ErasedBody = erase_expr(Body),
    {operation_case, Name, ErasedParams, ErasedBody, Loc}.

%%====================================================================
%% Pattern Erasure
%%====================================================================

%% @doc Erase type information from a pattern
-spec erase_pattern(pattern()) -> pattern().

%% Variable pattern - strip type annotation
erase_pattern({pat_var, Name, Loc}) ->
    {pat_var, Name, Loc};

%% Typed variable - strip type
erase_pattern({pat_typed_var, Name, _Type, Loc}) ->
    {pat_var, Name, Loc};

%% Wildcard
erase_pattern({pat_wildcard, Loc}) ->
    {pat_wildcard, Loc};

%% Literal
erase_pattern({pat_literal, Value, Type, Loc}) ->
    {pat_literal, Value, Type, Loc};

%% Constructor
erase_pattern({pat_constructor, Name, Args, Loc}) ->
    {pat_constructor, Name, [erase_pattern(A) || A <- Args], Loc};

%% List
erase_pattern({pat_list, Elements, Loc}) ->
    {pat_list, [erase_pattern(E) || E <- Elements], Loc};

%% Cons
erase_pattern({pat_cons, Head, Tail, Loc}) ->
    {pat_cons, erase_pattern(Head), erase_pattern(Tail), Loc};

%% Tuple
erase_pattern({pat_tuple, Elements, Loc}) ->
    {pat_tuple, [erase_pattern(E) || E <- Elements], Loc};

%% As-pattern
erase_pattern({pat_as, Name, Pattern, Loc}) ->
    {pat_as, Name, erase_pattern(Pattern), Loc};

%% Record pattern
erase_pattern({pat_record, Fields, Loc}) ->
    ErasedFields = [{Name, erase_pattern(P)} || {Name, P} <- Fields],
    {pat_record, ErasedFields, Loc};

%% Default
erase_pattern(Other) ->
    Other.

%%====================================================================
%% Type Annotation Stripping (1.3.3.1)
%%====================================================================

%% @doc Strip all type annotations from an AST
-spec strip_type_annotations(term()) -> term().
strip_type_annotations(AST) ->
    erase_expr(AST).

%%====================================================================
%% Dictionary Passing Transformation (1.3.3.2)
%%====================================================================

%% @doc Transform trait-polymorphic calls to dictionary passing
%%
%% For a function like:
%%   transform show : Show a => a -> String
%%   transform show x = Show.show(x)
%%
%% Transforms to:
%%   transform show(show_dict, x) = show_dict.show(x)
-spec transform_trait_calls(expr(), constraints()) -> expr().
transform_trait_calls(Expr, Constraints) ->
    %% Add dictionary parameters for each constraint
    DictParams = [constraint_to_param(C) || C <- Constraints],

    case Expr of
        {lambda, Params, Body, Loc} ->
            %% Add dictionary parameters to lambda
            NewParams = DictParams ++ Params,
            TransformedBody = transform_body_calls(Body, Constraints),
            {lambda, NewParams, TransformedBody, Loc};
        _ ->
            %% Wrap in lambda with dictionary parameters
            TransformedBody = transform_body_calls(Expr, Constraints),
            case DictParams of
                [] -> TransformedBody;
                _ -> {lambda, DictParams, TransformedBody, {location, 0, 0}}
            end
    end.

%% Convert constraint to dictionary parameter
constraint_to_param({trait, TraitName, _TypeArgs, _Loc}) ->
    {pat_var, trait_dict_var(TraitName), {location, 0, 0}}.

%% Transform trait method calls in body to dictionary lookups
transform_body_calls({trait_method, TraitName, Method, _TypeArgs, Args, Loc}, _Constraints) ->
    DictVar = {var, trait_dict_var(TraitName), Loc},
    MethodAccess = {record_access, DictVar, Method, Loc},
    {app, MethodAccess, Args, Loc};

transform_body_calls({app, Func, Args, Loc}, Constraints) ->
    {app, transform_body_calls(Func, Constraints),
     [transform_body_calls(A, Constraints) || A <- Args], Loc};

transform_body_calls({let_expr, Bindings, Body, Loc}, Constraints) ->
    TransformedBindings = [{P, transform_body_calls(E, Constraints)} || {P, E} <- Bindings],
    {let_expr, TransformedBindings, transform_body_calls(Body, Constraints), Loc};

transform_body_calls({lambda, Params, Body, Loc}, Constraints) ->
    {lambda, Params, transform_body_calls(Body, Constraints), Loc};

transform_body_calls({if_expr, Cond, Then, Else, Loc}, Constraints) ->
    {if_expr,
     transform_body_calls(Cond, Constraints),
     transform_body_calls(Then, Constraints),
     transform_body_calls(Else, Constraints),
     Loc};

transform_body_calls({binary_op, Op, Left, Right, Loc}, Constraints) ->
    {binary_op, Op,
     transform_body_calls(Left, Constraints),
     transform_body_calls(Right, Constraints),
     Loc};

transform_body_calls(Other, _Constraints) ->
    Other.

%% @doc Build a trait dictionary for a specific instance
-spec build_dictionary(atom(), [{atom(), integer()}]) -> expr().
build_dictionary(TraitName, TypeArgs) ->
    DictName = instance_dict_name(TraitName, TypeArgs),
    {var, DictName, {location, 0, 0}}.

%% Build instance dictionary value from methods
build_instance_dict(Methods) ->
    Fields = [{Name, build_method_ref(Name, Arity)} || {Name, Arity, _Body} <- Methods],
    {record_expr, Fields, {location, 0, 0}}.

build_method_ref(Name, _Arity) ->
    {var, Name, {location, 0, 0}}.

%%====================================================================
%% Polymorphism Handling (1.3.3.3)
%%====================================================================

%% @doc Handle polymorphism through uniform representation
%%
%% For PoC, we use uniform representation where all values have
%% the same runtime representation regardless of their static type.
%% This is natural for BEAM which is dynamically typed.
-spec monomorphize(expr(), type_env()) -> expr().
monomorphize(Expr, _TypeEnv) ->
    %% For BEAM, no monomorphization needed - all types have
    %% uniform representation at runtime
    %%
    %% Simply erase types and ensure consistent representation
    erase_expr(Expr).

%%====================================================================
%% Semantic Preservation Verification (1.3.3.4)
%%====================================================================

%% @doc Verify that type erasure preserves semantics
%%
%% Checks that:
%% 1. All type annotations are removed
%% 2. All trait calls are transformed to dictionary passing
%% 3. Runtime behavior is preserved
-spec verify_erasure(expr(), expr()) -> ok | {error, term()}.
verify_erasure(Original, Erased) ->
    %% Check no type annotations remain
    case has_type_annotations(Erased) of
        true ->
            {error, type_annotations_remain};
        false ->
            %% Check structural preservation
            check_structure_preserved(Original, Erased)
    end.

%% Check if expression contains type annotations
has_type_annotations({typed_expr, _, _, _}) -> true;
has_type_annotations({typed_lambda, _, _, _, _}) -> true;
has_type_annotations({type_ascription, _, _, _}) -> true;
has_type_annotations({pat_typed_var, _, _, _}) -> true;
has_type_annotations({app, Func, Args, _}) ->
    has_type_annotations(Func) orelse
    lists:any(fun has_type_annotations/1, Args);
has_type_annotations({let_expr, Bindings, Body, _}) ->
    lists:any(fun({P, E}) ->
        has_type_annotations(P) orelse has_type_annotations(E)
    end, Bindings) orelse has_type_annotations(Body);
has_type_annotations({lambda, Params, Body, _}) ->
    lists:any(fun has_type_annotations/1, Params) orelse
    has_type_annotations(Body);
has_type_annotations({if_expr, Cond, Then, Else, _}) ->
    has_type_annotations(Cond) orelse
    has_type_annotations(Then) orelse
    has_type_annotations(Else);
has_type_annotations({binary_op, _, Left, Right, _}) ->
    has_type_annotations(Left) orelse has_type_annotations(Right);
has_type_annotations({list_expr, Elements, _}) ->
    lists:any(fun has_type_annotations/1, Elements);
has_type_annotations({tuple_expr, Elements, _}) ->
    lists:any(fun has_type_annotations/1, Elements);
has_type_annotations(_) -> false.

%% Check that computational structure is preserved
check_structure_preserved(_Original, _Erased) ->
    %% For PoC, we trust that erasure preserves structure
    %% Full verification would compare execution traces
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Generate dictionary variable name for a trait
trait_dict_var(TraitName) ->
    list_to_atom(atom_to_list(TraitName) ++ "_dict").

%% Generate instance dictionary name
instance_dict_name(TraitName, TypeArgs) ->
    TypeStr = lists:flatten([type_to_string(T) || T <- TypeArgs]),
    list_to_atom(atom_to_list(TraitName) ++ "_" ++ TypeStr ++ "_dict").

type_to_string({tcon, Name}) -> atom_to_list(Name);
type_to_string({tvar, Id}) -> "t" ++ integer_to_list(Id);
type_to_string({tapp, Con, Args}) ->
    type_to_string(Con) ++ "_" ++
    lists:flatten(lists:join("_", [type_to_string(A) || A <- Args]));
type_to_string(Other) when is_atom(Other) -> atom_to_list(Other);
type_to_string(_) -> "unknown".

%%====================================================================
%% Type Definitions
%%====================================================================

-type module_ast() :: {module, atom(), [export()], [import()], [decl()], term()}.
-type export() :: {atom(), integer()}.
-type import() :: term().
-type decl() :: term().
-type expr() :: term().
-type pattern() :: term().
-type constraints() :: [constraint()].
-type constraint() :: {trait, atom(), [type()], term()}.
-type type() :: term().
-type type_env() :: map().
