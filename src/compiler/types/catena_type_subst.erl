%%%
%%% @doc Type Substitution Operations
%%%
%%% Implements substitutions (mappings from type variables to types)
%%% and operations for composing and applying them. Essential for
%%% Robinson's unification algorithm and Algorithm W.
%%%
%%% @end
%%%
-module(catena_type_subst).

-compile({no_auto_import,[apply/2]}).

-export([
    empty/0,
    singleton/2,
    lookup/2,
    extend/3,
    compose/2,
    apply/2,
    apply_effects/2,
    domain/1,
    range/1,
    occurs_check/2
]).

%%====================================================================
%% Configuration
%%====================================================================

%% Maximum depth for type traversal (prevents stack overflow)
-define(MAX_SUBST_DEPTH, 500).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Substitution: mapping from type variable IDs to types
-type subst() :: #{catena_types:type_var_id() => catena_types:ty()}.

-export_type([subst/0]).

%%====================================================================
%% Substitution Construction
%%====================================================================

-spec empty() -> subst().
empty() ->
    #{}.

-spec singleton(catena_types:type_var_id(), catena_types:ty()) -> subst().
singleton(VarId, Type) when is_integer(VarId), VarId > 0 ->
    #{VarId => Type}.

-spec lookup(subst(), catena_types:type_var_id()) -> {ok, catena_types:ty()} | none.
lookup(Subst, VarId) ->
    case maps:find(VarId, Subst) of
        {ok, Type} -> {ok, Type};
        error -> none
    end.

-spec extend(subst(), catena_types:type_var_id(), catena_types:ty()) -> subst().
%% @doc Extend a substitution with a new variable-to-type mapping
%%
%% Validates that the extended substitution does not exceed the maximum
%% substitution size to prevent unbounded growth during type inference.
extend(Subst, VarId, Type) ->
    Result = maps:put(VarId, Type, Subst),

    % Validate size to prevent unbounded substitution growth
    ResultSize = maps:size(Result),
    MaxSize = catena_compiler_utils:get_max_substitution_size(),
    case ResultSize > MaxSize of
        true ->
            error(catena_type_error:substitution_too_large(ResultSize, MaxSize));
        false ->
            Result
    end.

%%====================================================================
%% Substitution Composition
%%====================================================================

-spec compose(subst(), subst()) -> subst().
%% @doc Compose two substitutions: compose(S2, S1) = S2 ∘ S1
%% This means: first apply S1, then apply S2
%% Result: a substitution that has the same effect as applying S1 then S2
%%
%% == Substitution Amplification ==
%%
%% Composing substitutions can cause size amplification: if S1 has N mappings
%% and S2 has M mappings, the result can have up to N+M mappings. To prevent
%% unbounded growth during type inference, this function validates that the
%% composed substitution does not exceed the configured maximum substitution
%% size (default: 10,000 mappings).
%%
%% If the limit is exceeded, an error is raised with details about the actual
%% size and the limit, allowing the user to either simplify their types or
%% increase the limit via configuration.
compose(S2, S1) ->
    % Apply S2 to all types in the range of S1
    S1Applied = maps:map(fun(_VarId, Type) -> apply(S2, Type) end, S1),
    % Merge: S2's bindings take precedence, but include S1's bindings too
    Result = maps:merge(S1Applied, S2),

    % Validate size to prevent substitution amplification
    ResultSize = maps:size(Result),
    MaxSize = catena_compiler_utils:get_max_substitution_size(),
    case ResultSize > MaxSize of
        true ->
            error(catena_type_error:substitution_too_large(ResultSize, MaxSize));
        false ->
            Result
    end.

%%====================================================================
%% Substitution Application
%%====================================================================

-spec apply(subst(), catena_types:ty()) -> catena_types:ty().
%% @doc Apply a substitution to a type, replacing type variables
%% Includes occurs check to detect circular substitutions and depth
%% tracking to prevent stack overflow.
apply(Subst, Type) ->
    apply_with_context(Subst, Type, 0, sets:new()).

%% Internal application with depth tracking and occurs check
-spec apply_with_context(subst(), catena_types:ty(), non_neg_integer(), sets:set(catena_types:type_var_id()))
    -> catena_types:ty().

apply_with_context(_Subst, _Type, Depth, _Visited) when Depth > ?MAX_SUBST_DEPTH ->
    error({substitution_depth_exceeded, Depth, ?MAX_SUBST_DEPTH});

apply_with_context(Subst, {tvar, VarId}, Depth, Visited) ->
    case lookup(Subst, VarId) of
        none ->
            {tvar, VarId};  % Variable not in substitution, keep as is
        {ok, {tvar, VarId}} ->
            % Identity substitution: α ↦ α (idempotent, not circular)
            {tvar, VarId};
        {ok, Type} ->
            % Check if we've already visited this variable (occurs check)
            case sets:is_element(VarId, Visited) of
                true ->
                    error({circular_substitution, VarId});
                false ->
                    % Add this variable to visited set and continue
                    NewVisited = sets:add_element(VarId, Visited),
                    apply_with_context(Subst, Type, Depth + 1, NewVisited)
            end
    end;

apply_with_context(_Subst, {tcon, Name}, _Depth, _Visited) ->
    {tcon, Name};  % Constants don't change

apply_with_context(Subst, {tapp, Constructor, Args}, Depth, Visited) ->
    NewConstructor = apply_with_context(Subst, Constructor, Depth + 1, Visited),
    NewArgs = [apply_with_context(Subst, Arg, Depth + 1, Visited) || Arg <- Args],
    {tapp, NewConstructor, NewArgs};

apply_with_context(Subst, {tfun, From, To, Effects}, Depth, Visited) ->
    NewFrom = apply_with_context(Subst, From, Depth + 1, Visited),
    NewTo = apply_with_context(Subst, To, Depth + 1, Visited),
    NewEffects = apply_effects(Subst, Effects),
    {tfun, NewFrom, NewTo, NewEffects};

apply_with_context(Subst, {trecord, Fields, RowVar}, Depth, Visited) ->
    NewFields = [{Name, apply_with_context(Subst, FieldType, Depth + 1, Visited)}
                 || {Name, FieldType} <- Fields],
    NewRowVar = case RowVar of
                    closed -> closed;
                    VarId when is_integer(VarId) ->
                        % Check occurs for row variable
                        case sets:is_element(VarId, Visited) of
                            true -> error({circular_substitution, VarId});
                            false ->
                                case lookup(Subst, VarId) of
                                    {ok, {tvar, NewVarId}} -> NewVarId;
                                    {ok, _Other} -> closed;  % Row variable bound to non-var
                                    none -> VarId
                                end
                        end
                end,
    {trecord, NewFields, NewRowVar};

apply_with_context(Subst, {ttuple, Elements}, Depth, Visited) ->
    NewElements = [apply_with_context(Subst, Elem, Depth + 1, Visited) || Elem <- Elements],
    {ttuple, NewElements};

apply_with_context(Subst, {tvariant, Constructors}, Depth, Visited) ->
    NewConstructors = [{Name, [apply_with_context(Subst, ArgType, Depth + 1, Visited)
                               || ArgType <- ArgTypes]}
                      || {Name, ArgTypes} <- Constructors],
    {tvariant, NewConstructors}.

-spec apply_effects(subst(), catena_types:effect_set()) -> catena_types:effect_set().
%% @doc Apply substitution to effect set
%% Effect sets don't contain type variables currently, so this is identity
%% But included for future effect polymorphism
apply_effects(_Subst, Effects) ->
    Effects.

%%====================================================================
%% Substitution Properties
%%====================================================================

-spec domain(subst()) -> [catena_types:type_var_id()].
%% @doc Get the domain (set of variables) of a substitution
domain(Subst) ->
    maps:keys(Subst).

-spec range(subst()) -> [catena_types:ty()].
%% @doc Get the range (set of types) of a substitution
range(Subst) ->
    maps:values(Subst).

%%====================================================================
%% Occurs Check
%%====================================================================

-spec occurs_check(catena_types:type_var_id(), catena_types:ty()) -> boolean().
%% @doc Check if a type variable occurs in a type
%% Returns true if the variable occurs (which means substitution would create a cycle)
%% Returns false if the variable does not occur (safe to substitute)
%%
%% This is the classic "occurs check" from unification theory.
%% Example: occurs_check(1, List<α₁>) returns true (α₁ occurs in List<α₁>)
occurs_check(VarId, Type) ->
    TypeVars = catena_types:type_vars(Type),
    sets:is_element(VarId, TypeVars).

%%====================================================================
%% Internal Functions
%%====================================================================

% (Internal functions moved to top of file as apply_with_context/4)
