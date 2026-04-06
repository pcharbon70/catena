%% @doc Law Testing Framework - Record Definitions
%%
%% This file defines the core records for the law testing framework.

%%====================================================================
%% Law Records
%%====================================================================

%% A single mathematical law that a trait instance must satisfy.
-record(law, {
    name :: binary(),
    description :: binary(),
    property_fn :: fun()
}).

%% Parameters for instantiating a law with concrete types.
-record(law_params, {
    generator :: catena_gen:generator(_),
    eq_fn :: fun((_, _) -> boolean()),
    extra_gens :: #{atom() => catena_gen:generator(_)}
}).

%% A set of laws for a trait.
-record(law_set, {
    name :: binary(),
    laws :: [catena_laws:law()]
}).

%%====================================================================
%% Discipline Records
%%====================================================================

%% A discipline packages laws for testing a specific trait.
-record(discipline, {
    trait_name :: atom(),
    laws :: [catena_laws:law()],
    requires :: [atom()],
    auxiliary :: #{atom() => catena_gen:generator(_)}
}).
