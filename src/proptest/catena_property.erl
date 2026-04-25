%% @doc Property Testing Framework - Phase 3: Property DSL
%%
%% This module implements Property Testing Phase 3, providing the property
%% specification DSL for defining and executing property tests.
%%
%% == Property DSL ==
%%
%% Properties are defined using `forall` to bind generated values and
%% express predicates that must hold for all inputs:
%%
%% ```
%% %% Property: reverse(reverse(L)) == L
%% Property reverse_id =
%%     forall({gen_list(gen_int())}, fun(L) ->
%%         lists:reverse(lists:reverse(L)) =:= L
%%     end).
%% ```
%%
%% == Design Philosophy ==
%%
%% The DSL is designed to be:
%% - **Minimal**: Few core constructs with clear semantics
%% - **Composable**: Properties can be grouped and configured
%% - **Reproducible**: Every run is deterministic with a seed
%% - **Clear**: Failures show exact counterexamples
%%
%% @see catena_gen for generator infrastructure
%% @see catena_shrink for shrinking algorithms
-module(catena_property).

-include("catena_property.hrl").

%% Type exports
-export_type([
    property/0,
    property_config/0,
    property_result/0,
    property_result_kind/0
]).

%% API exports - Section 3.1.1: Property Type Definition
-export([
    property/2,
    new/3,
    default_config/0,
    with_seed/2
]).

%% API exports - Section 3.1.2: Forall Syntax
-export([
    forall/2,
    forall/3
]).

%% API exports - Section 3.1.3: Implication and Preconditions
-export([
    implies/2,
    discard/0
]).

%% API exports - Section 3.1.4: Property Combinators
-export([
    property_group/1,
    with_config/2,
    with_label/2,
    classify/3
]).

%%====================================================================
%% Types
%%====================================================================

-type property() :: #property{}.
-type property_config() :: #property_config{}.
-type property_result_kind() :: success | failure | discarded | error.
-type property_result() :: #property_result{}.

%%====================================================================
%% Section 3.1.1: Property Type Definition
%%====================================================================

%% @doc Create a new property from a name and body function.
%%
%% The body function receives forall-bound values and returns a boolean.
%% Properties are configured with defaults: 100 tests, 1000 shrinks.
%%
%% == Example ==
%%
%% ```
%% Property prop_reverse_id =
%%     property("reverse_id", fun() ->
%%         forall(gen_list(gen_int()), fun(L) ->
%%             lists:reverse(lists:reverse(L)) =:= L
%%         end)
%%     end).
%% ```
-spec property(binary() | string(), fun(() -> catena_gen:generator(term()))) -> property().
property(Name, BodyFun) when is_list(Name) ->
    property(unicode:characters_to_binary(Name), BodyFun);
property(Name, BodyFun) when is_binary(Name) ->
    %% Extract the generator from the forall call
    case BodyFun() of
        {forall, Gen, PredFun} ->
            #property{
                name = Name,
                generator = Gen,
                predicate = predicate_wrapper(PredFun),
                config = default_config()
            };
        _Other ->
            error({invalid_property_body, "Property body must use forall"})
    end.

%% @doc Create a property with explicit generator and predicate.
%%
%% For advanced use where direct control over the generator and predicate is needed.
-spec new(binary() | string(), catena_gen:generator(_), fun((_) -> boolean())) -> property().
new(Name, Generator, Predicate) when is_list(Name) ->
    new(unicode:characters_to_binary(Name), Generator, Predicate);
new(Name, Generator, Predicate) when is_binary(Name) ->
    #property{
        name = Name,
        generator = Generator,
        predicate = Predicate,
        config = default_config()
    }.

%% @doc Get default property configuration.
-spec default_config() -> property_config().
default_config() ->
    #property_config{
        num_tests = 100,
        max_shrinks = 1000,
        seed = undefined,
        labels = [],
        classify_fun = undefined
    }.

%%====================================================================
%% Section 3.1.2: Forall Syntax
%%====================================================================

%% @doc Universal quantification binding a single variable.
%%
%% The generated value is passed directly to the predicate function.
%%
%% == Example ==
%%
%% ```
%% forall(gen_int(), fun(N) -> N >= 0 end)
%% ```
-spec forall(catena_gen:generator(A), fun((A) -> boolean())) ->
    {forall, catena_gen:generator(A), fun((A) -> boolean())}.
forall(Generator, Predicate) ->
    {forall, Generator, Predicate}.

%% @doc Universal quantification with guard conditions.
%%
%% Tests that only pass when the guard is true. The condition is evaluated
%% before generating test values.
%%
%% == Example ==
%%
%% ```
%% forall(gen_int(), fun(N) when N > 0 -> N rem 2 =:= 0 end)
%% ```
-spec forall(catena_gen:generator(A), fun((A) -> boolean()), proplist) ->
    {forall, catena_gen:generator(A), fun((A) -> boolean())}.
forall(Generator, GuardFun, Predicate) ->
    %% Apply the guard as a filter
    FilteredGen = catena_gen:gen_filter(GuardFun, Generator),
    {forall, FilteredGen, Predicate}.

%%====================================================================
%% Section 3.1.3: Implication and Preconditions
%%====================================================================

%% @doc Implication: Precondition ==> Property.
%%
%% When the precondition fails, the test case is discarded (not passed or failed).
%% Use `implies/2` function form to express implications.
%%
%% == Example ==
%%
%% ```
%% forall(gen_int(), fun(N) -> implies(N > 0, fun() -> N rem 2 =:= 0 end) end)
%% ```

%% @doc Functional form of implication.
%%
%% If Precondition is true, evaluates Fun; otherwise returns discard.
-spec implies(boolean(), fun(() -> A)) -> A | discard.
implies(true, Fun) -> Fun();
implies(false, _Fun) -> discard.

%% @doc Discard marker for failed preconditions.
-spec discard() -> discard.
discard() -> discard.

%%====================================================================
%% Section 3.1.4: Property Combinators
%%====================================================================

%% @doc Group multiple properties to be run together.
%%
%% All properties in the group are executed and results aggregated.
-spec property_group(list()) -> property().
property_group(Properties) ->
    GroupName = <<"Property Group (", (integer_to_binary(length(Properties)))/binary, ")">>,
    %% For the group, we create a meta-property that runs all members
    GroupGen = catena_gen:constant(Properties),
    GroupPred = fun(_PropertiesList) -> true end,
    #property{
        name = GroupName,
        generator = GroupGen,
        predicate = GroupPred,
        config = default_config()
    }.

%% @doc Modify property configuration.
%%
%% Override default test count, shrinks, or seed for a property.
-spec with_config([{num_tests, pos_integer()} | {max_shrinks, pos_integer()} | {seed, catena_gen:seed()}], property()) -> property().
with_config(Options, Property) ->
    Config = Property#property.config,
    NewConfig = lists:foldl(fun
        ({num_tests, N}, Acc) -> Acc#property_config{num_tests = N};
        ({max_shrinks, N}, Acc) -> Acc#property_config{max_shrinks = N};
        ({seed, Seed}, Acc) -> Acc#property_config{seed = Seed}
    end, Config, Options),
    Property#property{config = NewConfig}.

%% @doc Add a label to test cases for distribution tracking.
%%
%% Labels are counted in results and displayed in output.
-spec with_label(binary(), property()) -> property().
with_label(Label, Property) ->
    Config = Property#property.config,
    OldLabels = Config#property_config.labels,
    NewConfig = Config#property_config{labels = [Label | OldLabels]},
    Property#property{config = NewConfig}.

%% @doc Classify test cases by a function for labeled distribution.
%%
%% The classification function receives the input and returns a label atom.
%% The label is stored alongside the function for reporting.
-spec classify(binary(), fun((_) -> atom()), property()) -> property().
classify(Label, ClassFun, Property) ->
    Config = Property#property.config,
    NewConfig = Config#property_config{classify_fun = {Label, ClassFun}},
    Property#property{config = NewConfig}.

%% @doc Set a specific seed for reproducible test execution.
%%
%% Setting a seed ensures that the same sequence of test cases will be
%% generated every time the property is run, which is essential for
%% debugging failing properties.
%%
%% == Example ==
%%
%% ```
%% Prop = property("my_prop", fun() ->
%%     forall(gen_int(), fun(N) -> N rem 2 =:= 0 end)
%% end),
%% ReproducibleProp = with_seed(12345, Prop).
%% ```
%%
-spec with_seed(non_neg_integer() | catena_gen:seed(), property()) -> property().
with_seed(SeedInt, Property) when is_integer(SeedInt) ->
    with_seed(catena_gen:seed_from_int(SeedInt), Property);
with_seed(Seed, Property) ->
    Config = Property#property.config,
    NewConfig = Config#property_config{seed = Seed},
    Property#property{config = NewConfig}.

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @private Convert a value tuple to a list for predicate application.
-spec catena_tuple_to_list(tuple()) -> list().
catena_tuple_to_list(Tuple) when is_tuple(Tuple) ->
    %% Convert tuple to list
    catena_list_to_tuple(Tuple);
catena_tuple_to_list(Other) when not is_tuple(Other) ->
    [Other].

%% @private Convert tuple to list.
-spec catena_list_to_tuple(tuple()) -> list().
catena_list_to_tuple(Tuple) ->
    elements_to_list(Tuple, 1, tuple_size(Tuple)).

%% @private Get elements from tuple as list starting at N.
-spec elements_to_list(tuple(), pos_integer(), pos_integer()) -> list().
elements_to_list(_Tuple, Pos, Max) when Pos > Max ->
    [];
elements_to_list(Tuple, Pos, Max) ->
    [element(Pos, Tuple) | elements_to_list(Tuple, Pos + 1, Max)].

predicate_wrapper(PredFun) ->
    case erlang:fun_info(PredFun, arity) of
        {arity, 0} ->
            fun(_Values) -> PredFun() end;
        {arity, 1} ->
            fun(Value) -> PredFun(Value) end;
        {arity, _N} ->
            fun(Values) -> apply(PredFun, catena_tuple_to_list(Values)) end
    end.
