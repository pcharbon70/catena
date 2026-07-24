%%%-------------------------------------------------------------------
%%% @doc Stable diagnostics for the Core Erlang and BEAM backend.
%%%
%%% Backend stages raise these diagnostics internally. The module generation
%%% boundary converts them to `{error, Diagnostic}` while preserving errors
%%% returned by the lexer, parser, semantic analysis, and type checker.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_backend_error).

-export([
    categories/0,
    new/2,
    context/3,
    context/4,
    unsupported_backend_construct/2,
    unresolved_call/3,
    ambiguous_call/4,
    arity_mismatch/4,
    missing_transform_implementation/2,
    invalid_declaration_disposition/2,
    core_validation_failed/3,
    beam_compilation_failed/3,
    is_diagnostic/1,
    category/1,
    details/1,
    source_location/1,
    format/1
]).

-type category() ::
    unsupported_backend_construct |
    unresolved_call |
    ambiguous_call |
    arity_mismatch |
    missing_transform_implementation |
    invalid_declaration_disposition |
    core_validation_failed |
    beam_compilation_failed.

-type diagnostic() :: {backend_error, category(), map()}.

-export_type([category/0, diagnostic/0]).

%% @doc Return the stable categories accepted by the backend contract.
-spec categories() -> [category()].
categories() ->
    [
        unsupported_backend_construct,
        unresolved_call,
        ambiguous_call,
        arity_mismatch,
        missing_transform_implementation,
        invalid_declaration_disposition,
        core_validation_failed,
        beam_compilation_failed
    ].

%% @doc Construct a backend diagnostic with a stable context envelope.
-spec new(category(), map()) -> diagnostic().
new(Category, Details) when is_map(Details) ->
    true = lists:member(Category, categories()),
    {backend_error, Category, maps:merge(default_context(), Details)}.

%% @doc Build source context from a stage, construct kind, and source term.
-spec context(atom(), atom(), term()) -> map().
context(Stage, Construct, SourceTerm) ->
    context(Stage, Construct, SourceTerm, #{}).

%% @doc Build source context and add known module or transform identities.
-spec context(atom(), atom(), term(), map()) -> map().
context(Stage, Construct, SourceTerm, Extra) when is_map(Extra) ->
    maps:merge(
        #{
            stage => Stage,
            construct => Construct,
            location => source_location(SourceTerm)
        },
        Extra
    ).

-spec unsupported_backend_construct(term(), map()) -> diagnostic().
unsupported_backend_construct(Construct, Context) ->
    new(
        unsupported_backend_construct,
        Context#{construct => Construct}
    ).

-spec unresolved_call(atom(), non_neg_integer(), map()) -> diagnostic().
unresolved_call(Name, Arity, Context) ->
    new(
        unresolved_call,
        Context#{
            construct => call,
            source_identity => {Name, Arity}
        }
    ).

-spec ambiguous_call(atom(), non_neg_integer(), [term()], map()) -> diagnostic().
ambiguous_call(Name, Arity, Candidates, Context) ->
    new(
        ambiguous_call,
        Context#{
            construct => call,
            source_identity => {Name, Arity},
            candidates => Candidates
        }
    ).

-spec arity_mismatch(atom(), non_neg_integer(), non_neg_integer(), map()) ->
    diagnostic().
arity_mismatch(Name, Expected, Actual, Context) ->
    new(
        arity_mismatch,
        Context#{
            construct => call,
            source_identity => Name,
            expected_arity => Expected,
            actual_arity => Actual
        }
    ).

-spec missing_transform_implementation(atom(), map()) -> diagnostic().
missing_transform_implementation(Name, Context) ->
    new(
        missing_transform_implementation,
        Context#{
            construct => transform,
            source_identity => Name
        }
    ).

-spec invalid_declaration_disposition(term(), map()) -> diagnostic().
invalid_declaration_disposition(Declaration, Context) ->
    new(
        invalid_declaration_disposition,
        Context#{
            construct => declaration,
            declaration => Declaration
        }
    ).

-spec core_validation_failed(term(), term(), map()) -> diagnostic().
core_validation_failed(Errors, Warnings, Context) ->
    new(
        core_validation_failed,
        Context#{
            construct => core_module,
            errors => Errors,
            warnings => Warnings
        }
    ).

-spec beam_compilation_failed(term(), term(), map()) -> diagnostic().
beam_compilation_failed(Errors, Warnings, Context) ->
    new(
        beam_compilation_failed,
        Context#{
            construct => beam_module,
            errors => Errors,
            warnings => Warnings
        }
    ).

-spec is_diagnostic(term()) -> boolean().
is_diagnostic({backend_error, Category, Details}) when is_map(Details) ->
    lists:member(Category, categories());
is_diagnostic(_) ->
    false.

-spec category(diagnostic()) -> category().
category({backend_error, Category, _Details}) ->
    Category.

-spec details(diagnostic()) -> map().
details({backend_error, _Category, Details}) ->
    Details.

%% @doc Extract a source location from a context map or AST tuple.
-spec source_location(term()) -> term().
source_location(#{location := Location}) ->
    Location;
source_location({location, _Line, _Column} = Location) ->
    Location;
source_location(Term) when is_tuple(Term), tuple_size(Term) > 1 ->
    Candidate = element(tuple_size(Term), Term),
    case is_location(Candidate) of
        true -> Candidate;
        false -> undefined
    end;
source_location(_) ->
    undefined.

%% @doc Format a backend diagnostic with Catena source identities.
-spec format(diagnostic()) -> string().
format({backend_error, Category, Details}) ->
    lists:flatten(
        io_lib:format(
            "Catena backend ~s~s~s",
            [
                category_message(Category, Details),
                format_identity(Details),
                format_location(maps:get(location, Details, undefined))
            ]
        )
    ).

default_context() ->
    #{
        stage => backend,
        construct => undefined,
        location => undefined,
        module => undefined,
        transform => undefined,
        generated_identity => undefined
    }.

is_location({location, Line, Column})
  when is_integer(Line), is_integer(Column) ->
    true;
is_location({Line, Column}) when is_integer(Line), is_integer(Column) ->
    true;
is_location(Line) when is_integer(Line) ->
    true;
is_location(_) ->
    false.

category_message(unsupported_backend_construct, Details) ->
    io_lib:format(
        "does not support the ~p construct",
        [maps:get(construct, Details, undefined)]
    );
category_message(unresolved_call, _Details) ->
    "could not resolve transform call";
category_message(ambiguous_call, _Details) ->
    "found an ambiguous transform call";
category_message(arity_mismatch, Details) ->
    io_lib:format(
        "found call arity ~p but expected ~p",
        [
            maps:get(actual_arity, Details, undefined),
            maps:get(expected_arity, Details, undefined)
        ]
    );
category_message(missing_transform_implementation, _Details) ->
    "requires an implementation for transform";
category_message(invalid_declaration_disposition, _Details) ->
    "could not classify a declaration for emission";
category_message(core_validation_failed, _Details) ->
    "generated Core Erlang that OTP rejected";
category_message(beam_compilation_failed, _Details) ->
    "could not produce a BEAM binary".

format_identity(#{module := undefined, transform := undefined} = Details) ->
    format_source_identity(Details);
format_identity(Details) ->
    Module = maps:get(module, Details, undefined),
    Transform = maps:get(transform, Details, undefined),
    io_lib:format(" in ~s", [source_scope(Module, Transform)]).

format_source_identity(#{source_identity := Identity}) ->
    io_lib:format(" for ~p", [Identity]);
format_source_identity(_) ->
    "".

source_scope(undefined, Transform) ->
    lists:flatten(io_lib:format("transform ~p", [Transform]));
source_scope(Module, undefined) ->
    lists:flatten(io_lib:format("module ~p", [Module]));
source_scope(Module, Transform) ->
    lists:flatten(
        io_lib:format("module ~p, transform ~p", [Module, Transform])
    ).

format_location({location, Line, Column}) ->
    io_lib:format(" at line ~p, column ~p", [Line, Column]);
format_location({Line, Column}) when is_integer(Line), is_integer(Column) ->
    io_lib:format(" at line ~p, column ~p", [Line, Column]);
format_location(Line) when is_integer(Line) ->
    io_lib:format(" at line ~p", [Line]);
format_location(_) ->
    "".
