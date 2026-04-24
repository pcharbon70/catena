%% @doc Adapter from legacy `property_decl` AST nodes into `src/proptest`
%% property values.
%%
%% Phase 1.1 keeps the current property syntax and runtime environment model,
%% but converts declarations into `catena_property` values so the property
%% engine can be swapped independently from the parser surface.
-module(catena_property_adapter).

-export([
    from_property_decl/2,
    from_property_body/3,
    bindings_generator/1,
    binding_generator/1
]).

-type property_error() ::
    {unsupported_property_decl, term()} |
    {unsupported_property_body, term()} |
    {unsupported_property_generator, atom()}.

-export_type([property_error/0]).

%%====================================================================
%% Public API
%%====================================================================

-spec from_property_decl(term(), map()) ->
    {ok, catena_property:property()} | {error, property_error()}.
from_property_decl({property_decl, Name, Body, _Loc}, Env) ->
    from_property_body(Name, Body, Env);
from_property_decl(Other, _Env) ->
    {error, {unsupported_property_decl, Other}}.

-spec from_property_body(string() | binary(), term(), map()) ->
    {ok, catena_property:property()} | {error, property_error()}.
from_property_body(Name, {property_forall, Bindings, Expr, _Loc}, Env) ->
    case bindings_generator(Bindings) of
        {ok, Generator} ->
            Predicate = property_predicate(Bindings, Expr, Env),
            {ok, catena_property:new(Name, Generator, Predicate)};
        {error, _} = Error ->
            Error
    end;
from_property_body(_Name, Other, _Env) ->
    {error, {unsupported_property_body, Other}}.

-spec bindings_generator([{atom(), atom()}]) ->
    {ok, catena_gen:generator(map())} | {error, property_error()}.
bindings_generator(Bindings) ->
    lists:foldl(
        fun(Binding, {ok, AccGen}) ->
                case binding_generator(Binding) of
                    {ok, BindingGen} ->
                        {ok, catena_gen:gen_map2(
                            fun(Acc, Value) ->
                                maps:merge(Acc, Value)
                            end,
                            AccGen,
                            BindingGen
                        )};
                    {error, _} = Error ->
                        Error
                end;
           (_Binding, {error, _} = Error) ->
                Error
        end,
        {ok, catena_gen:constant(#{} )},
        Bindings
    ).

-spec binding_generator({atom(), atom()}) ->
    {ok, catena_gen:generator(map())} | {error, property_error()}.
binding_generator({VarName, GeneratorName}) ->
    case generator_for_name(GeneratorName) of
        {ok, Generator} ->
            {ok, catena_gen:gen_map(
                fun(Value) ->
                    #{VarName => wrap_runtime_value(Value)}
                end,
                Generator
            )};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Internal Helpers
%%====================================================================

-spec property_predicate([{atom(), atom()}], term(), map()) -> fun((map()) -> term()).
property_predicate(_Bindings, Expr, Env) ->
    fun(GeneratedBindings) ->
        PropertyEnv = maps:merge(Env, GeneratedBindings),
        catena_test_runner:evaluate_expr(Expr, PropertyEnv)
    end.

-spec generator_for_name(atom()) ->
    {ok, catena_gen:generator(term())} | {error, property_error()}.
generator_for_name('Bool') ->
    {ok, catena_gen:gen_bool()};
generator_for_name('Natural') ->
    {ok, catena_gen:gen_nat()};
generator_for_name('Int') ->
    {ok, catena_gen:gen_int()};
generator_for_name('Integer') ->
    {ok, catena_gen:gen_int()};
generator_for_name('Text') ->
    {ok, catena_stdgen:gen_string(catena_range:range_constant({0, 20}))};
generator_for_name('String') ->
    {ok, catena_stdgen:gen_string(catena_range:range_constant({0, 20}))};
generator_for_name('List') ->
    {ok, catena_stdgen:gen_list(catena_gen:gen_int())};
generator_for_name('Maybe') ->
    {ok, catena_stdgen:gen_maybe(catena_gen:gen_int())};
generator_for_name('Result') ->
    {ok, catena_stdgen:gen_result(
        catena_gen:gen_int(),
        catena_stdgen:gen_string(catena_range:range_constant({0, 20}))
    )};
generator_for_name(Other) ->
    {error, {unsupported_property_generator, Other}}.

-spec wrap_runtime_value(term()) -> {value, term()}.
wrap_runtime_value(Value) ->
    {value, Value}.
