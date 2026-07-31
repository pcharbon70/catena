%%%-------------------------------------------------------------------
%%% @doc Compiler-backed Catena REPL sessions.
%%%
%%% A session accumulates source declarations, recompiles one bounded module,
%%% validates and loads the resulting artifact, and evaluates expressions in
%%% the owning process. It therefore exercises the same source-to-BEAM path as
%%% normal compilation, including `with`, `resume`, handler depth, and
%%% resumption ownership.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_repl_session).

-export([
    new/0,
    new/1,
    define/2,
    evaluate/2,
    bind/4,
    resume/2,
    resume/3,
    inspect/2,
    describe/1,
    describe_value/1,
    close/1
]).

-define(SESSION_VERSION, 1).
-define(DEFAULT_MODULE, 'CatenaReplSession').
-define(EVAL_TRANSFORM, repl_eval).

-opaque session() :: #{
    '$catena_repl_session' := ?SESSION_VERSION,
    owner := pid(),
    module := atom(),
    declarations := [string()],
    generation := non_neg_integer(),
    compiler_options := map(),
    runtime_bindings := map(),
    artifact => map()
}.

-export_type([session/0]).

-spec new() -> {ok, session()}.
new() ->
    new(#{}).

-spec new(map()) -> {ok, session()} | {error, term()}.
new(Options) when is_map(Options) ->
    Module = maps:get(module, Options, ?DEFAULT_MODULE),
    case catena_module_identity:normalize(Module) of
        {ok, _Identity} ->
            CompilerOptions = maps:get(compiler_options, Options, #{}),
            case is_map(CompilerOptions) of
                true ->
                    {ok, #{
                        '$catena_repl_session' => ?SESSION_VERSION,
                        owner => self(),
                        module => Module,
                        declarations => [],
                        generation => 0,
                        compiler_options => CompilerOptions,
                        runtime_bindings => #{}
                    }};
                false ->
                    {error, {invalid_repl_compiler_options, CompilerOptions}}
            end;
        {error, _} = Error ->
            Error
    end;
new(Options) ->
    {error, {invalid_repl_session_options, Options}}.

%% @doc Add one or more declarations and recompile the session module.
-spec define(string() | binary(), session()) ->
    {ok, map(), session()} | {error, term(), session()}.
define(Source0, Session) ->
    with_owner(Session, fun() ->
        case normalize_source(Source0) of
            {ok, Source} ->
                Declarations = maps:get(declarations, Session) ++ [Source],
                Candidate = module_source(Session, Declarations, none),
                case compile_artifact(Candidate, Session) of
                    {ok, TypedModule, Artifact} ->
                        case load_artifact(Artifact, Session) of
                            ok ->
                                Generation = maps:get(generation, Session) + 1,
                                Session1 = Session#{
                                    declarations := Declarations,
                                    generation := Generation,
                                    artifact => Artifact
                                },
                                {ok, #{
                                    generation => Generation,
                                    definitions => declaration_names(TypedModule),
                                    frames => catena_control_diagnostics:
                                        source_frames(Artifact)
                                }, Session1};
                            {error, Reason} ->
                                {error, diagnostic(Reason, Artifact), Session}
                        end;
                    {error, Reason} ->
                        {error, diagnostic(Reason, none), Session}
                end;
            {error, Reason} ->
                {error, Reason, Session}
        end
    end).

%% @doc Compile and execute an expression inside the accumulated session.
-spec evaluate(string() | binary(), session()) ->
    {ok, map(), session()} | {error, term(), session()}.
evaluate(Expression0, Session) ->
    with_owner(Session, fun() ->
        case normalize_source(Expression0) of
            {ok, Expression} ->
                Candidate = module_source(
                    Session,
                    maps:get(declarations, Session),
                    Expression
                ),
                case compile_artifact(Candidate, Session) of
                    {ok, TypedModule, Artifact} ->
                        case load_artifact(Artifact, Session) of
                            ok ->
                                Generation = maps:get(generation, Session) + 1,
                                Session1 = Session#{
                                    generation := Generation,
                                    artifact => Artifact
                                },
                                execute_expression(
                                    TypedModule,
                                    Artifact,
                                    Generation,
                                    Session1
                                );
                            {error, Reason} ->
                                {error, diagnostic(Reason, Artifact), Session}
                        end;
                    {error, Reason} ->
                        {error, diagnostic(Reason, none), Session}
                end;
            {error, Reason} ->
                {error, Reason, Session}
        end
    end).

%% @doc Preserve an externally produced typed runtime value in this session.
%% This is used by embedders that evaluate retained values at a surrounding
%% runtime boundary while keeping all subsequent operations owner-affine.
-spec bind(atom(), term(), term(), session()) ->
    {ok, session()} | {error, term()}.
bind(Name, Value, Type, Session) when is_atom(Name) ->
    with_owner(Session, fun() ->
        {ok, bind_runtime(Name, Value, Type, Session)}
    end);
bind(Name, _Value, _Type, _Session) ->
    {error, {invalid_runtime_binding_name, Name}}.

%% @doc Resume the session's conventional `it` resumption binding.
-spec resume(term(), session()) ->
    {ok, map(), session()} | {error, term(), session()} | {error, term()}.
resume(Value, Session) ->
    resume(it, Value, Session).

%% @doc Resume a named first-class resumption on the session owner process.
-spec resume(atom(), term(), session()) ->
    {ok, map(), session()} | {error, term(), session()} | {error, term()}.
resume(Name, Value, Session) when is_atom(Name) ->
    with_owner(Session, fun() ->
        Bindings = maps:get(runtime_bindings, Session),
        case maps:find(Name, Bindings) of
            {ok, #{value := Resumption}} ->
                case catena_resumption_runtime:resume(Resumption, Value) of
                    {ok, Result} ->
                        Session1 = bind_runtime(it, Result, unknown, Session),
                        {ok, #{
                            binding => it,
                            value => describe_value(Result)
                        }, Session1};
                    {error, Failure} ->
                        Artifact = maps:get(artifact, Session, none),
                        {error, diagnostic(Failure, Artifact), Session}
                end;
            error ->
                {error, {unknown_runtime_binding, Name}, Session}
        end
    end);
resume(Name, _Value, Session) ->
    {error, {invalid_runtime_binding_name, Name}, Session}.

%% @doc Inspect a named binding without returning its runtime value.
-spec inspect(atom(), session()) -> {ok, map()} | {error, term()}.
inspect(Name, Session) when is_atom(Name) ->
    with_owner(Session, fun() ->
        case maps:find(Name, maps:get(runtime_bindings, Session)) of
            {ok, #{value := Value, type := Type}} ->
                {ok, #{
                    binding => Name,
                    type => Type,
                    value => describe_value(Value)
                }};
            error ->
                {error, {unknown_runtime_binding, Name}}
        end
    end);
inspect(Name, _Session) ->
    {error, {invalid_runtime_binding_name, Name}}.

%% @doc Return non-sensitive session metadata.
-spec describe(session()) -> {ok, map()} | {error, term()}.
describe(Session) ->
    with_owner(Session, fun() ->
        {ok, #{
            module => maps:get(module, Session),
            generation => maps:get(generation, Session),
            declaration_count => length(maps:get(declarations, Session)),
            runtime_bindings => maps:keys(maps:get(runtime_bindings, Session)),
            loaded => maps:is_key(artifact, Session),
            owner_relationship => current_process
        }}
    end).

%% @doc Safely describe a value, recognizing first-class resumptions.
-spec describe_value(term()) ->
    #{kind := value, value := term()} |
    #{kind := resumption, description := map()} |
    #{kind := resumption, failure := map()}.
describe_value(Value) ->
    case catena_resumption_runtime:is_resumption(Value) of
        true ->
            case catena_resumption_runtime:describe(Value) of
                {ok, Description} ->
                    #{kind => resumption, description => Description};
                {error, Failure} ->
                    #{kind => resumption, failure => diagnostic(Failure, none)}
            end;
        false ->
            #{kind => value, value => Value}
    end.

%% @doc Unload the bounded session module. The owning process remains intact.
-spec close(session()) -> ok | {error, term()}.
close(Session) ->
    with_owner(Session, fun() ->
        Module = maps:get(module, Session),
        _ = code:soft_purge(Module),
        _ = code:delete(Module),
        _ = code:purge(Module),
        ok
    end).

with_owner(
    #{'$catena_repl_session' := ?SESSION_VERSION, owner := Owner},
    Fun
) when Owner =:= self(), is_function(Fun, 0) ->
    Fun();
with_owner(#{'$catena_repl_session' := ?SESSION_VERSION}, _Fun) ->
    {error, wrong_repl_session_owner};
with_owner(_Session, _Fun) ->
    {error, invalid_repl_session}.

normalize_source(Source) when is_binary(Source) ->
    normalize_source(binary_to_list(Source));
normalize_source(Source) when is_list(Source) ->
    case string:trim(Source) of
        [] -> {error, empty_repl_input};
        Trimmed -> {ok, Trimmed ++ "\n"}
    end;
normalize_source(Source) ->
    {error, {invalid_repl_source, Source}}.

module_source(Session, Declarations, none) ->
    "module " ++ atom_to_list(maps:get(module, Session)) ++ "\n" ++
        lists:flatten(Declarations);
module_source(Session, Declarations, Expression) ->
    "module " ++ atom_to_list(maps:get(module, Session)) ++ "\n" ++
        lists:flatten(Declarations) ++
        "transform " ++ atom_to_list(?EVAL_TRANSFORM) ++ " = " ++
        Expression.

compile_artifact(Source, Session) ->
    Options0 = maps:get(compiler_options, Session),
    Options = Options0#{source_identity => #{
        kind => repl_session,
        module => maps:get(module, Session),
        generation => maps:get(generation, Session) + 1
    }},
    case catena_compile:compile_string_to_unit(Source, Options) of
        {ok, Unit} ->
            TypedModule = catena_compilation_unit:typed_module(Unit),
            case catena_beam_artifact:from_unit(Unit) of
                {ok, Artifact} -> {ok, TypedModule, Artifact};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

load_artifact(Artifact, Session) ->
    Module = maps:get(module, Session),
    _ = code:soft_purge(Module),
    case catena_beam_artifact:load(Artifact) of
        {module, Module} -> ok;
        {error, _} = Error -> Error
    end.

execute_expression(TypedModule, Artifact, Generation, Session) ->
    Module = maps:get(runtime_module, Artifact),
    Type = transform_type(?EVAL_TRANSFORM, TypedModule),
    try erlang:apply(Module, ?EVAL_TRANSFORM, []) of
        {error, #{category := _} = Failure} ->
            {error, diagnostic(Failure, Artifact), Session};
        Value ->
            Session1 = bind_runtime(it, Value, Type, Session),
            {ok, #{
                generation => Generation,
                binding => it,
                type => Type,
                value => describe_value(Value),
                frames => catena_control_diagnostics:source_frames(Artifact)
            }, Session1}
    catch
        Class:Reason:_Stack ->
            Failure = catena_resumption_runtime:normalize_exception(
                Class,
                Reason,
                #{construct => repl_evaluation}
            ),
            {error, diagnostic(Failure, Artifact), Session}
    end.

bind_runtime(Name, Value, Type, Session) ->
    Bindings = maps:get(runtime_bindings, Session),
    Session#{runtime_bindings := Bindings#{Name => #{
        value => Value,
        type => Type
    }}}.

transform_type(Name, {typed_module, _Module, Declarations, _Env}) ->
    transform_type_from_declarations(Name, Declarations);
transform_type(_Name, _TypedModule) ->
    unknown.

transform_type_from_declarations(
    Name,
    [{typed_transform, Name, Type, _Clauses, _Location} | _]
) -> Type;
transform_type_from_declarations(
    Name,
    [{typed_transform, Name, Type, _Clauses, _Metadata, _Location} | _]
) -> Type;
transform_type_from_declarations(Name, [_ | Rest]) ->
    transform_type_from_declarations(Name, Rest);
transform_type_from_declarations(_Name, []) ->
    unknown.

declaration_names({typed_module, _Module, Declarations, _Env}) ->
    lists:usort(lists:append([
        declaration_name(Declaration)
        || Declaration <- Declarations
    ]));
declaration_names(_TypedModule) ->
    [].

declaration_name({typed_transform, Name, _Type, _Clauses, _Location}) ->
    [{transform, Name}];
declaration_name({typed_transform, Name, _Type, _Clauses, _Metadata, _Location}) ->
    [{transform, Name}];
declaration_name({effect_decl, Name, _Operations, _Location}) ->
    [{effect, Name}];
declaration_name({type_decl, Name, _Params, _Constructors, _Derives, _Location}) ->
    [{type, Name}];
declaration_name(_Declaration) ->
    [].

diagnostic(#{category := _} = Failure, Artifact) ->
    catena_control_diagnostics:failure_view(Failure, Artifact);
diagnostic(Reason, _Artifact) ->
    #{category => repl_pipeline_failure, details => #{reason => Reason}}.
