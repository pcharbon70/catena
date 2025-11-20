%%%-------------------------------------------------------------------
%%% @doc Type error explanations and fix suggestions
%%%
%%% This module provides human-readable explanations for type errors
%%% and suggests actionable fixes. It pattern-matches on error types
%%% to provide contextual help tailored to common mistakes.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_error_explain).

%% API exports
-export([
    explain/2,
    suggest_fix/2,
    explain_error/1,
    explain_error/2
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Get a human-readable explanation for a type error
-spec explain(catena_type_error:type_error(), map()) -> iolist().
explain(Error, Context) ->
    explain_error(Error, Context).

%% @doc Get actionable fix suggestions for a type error
-spec suggest_fix(catena_type_error:type_error(), map()) -> iolist().
suggest_fix({unification_error, T1, T2}, Context) ->
    suggest_type_mismatch_fix(T1, T2, Context);
suggest_fix({unsatisfied_constraint, Trait, Types, no_instance}, _Context) ->
    suggest_missing_instance_fix(Trait, Types);
suggest_fix({arity_mismatch, Expected, Actual, _FunName}, _Context) ->
    suggest_arity_fix(Expected, Actual);
suggest_fix({occurs_check, _Var, _Type}, _Context) ->
    suggest_infinite_type_fix();
suggest_fix({unbound_variable, Var}, _Context) ->
    suggest_unbound_variable_fix(Var);
suggest_fix({ambiguous_type, _Var}, _Context) ->
    suggest_ambiguous_type_fix();
suggest_fix(_Other, _Context) ->
    [].

%% @doc Explain an error with optional context
-spec explain_error(catena_type_error:type_error()) -> iolist().
explain_error(Error) ->
    explain_error(Error, #{}).

-spec explain_error(catena_type_error:type_error(), map()) -> iolist().
explain_error({unification_error, T1, T2}, Context) ->
    explain_type_mismatch(T1, T2, Context);
explain_error({unsatisfied_constraint, Trait, _Types, no_instance}, _Context) ->
    explain_missing_trait(Trait);
explain_error({arity_mismatch, Expected, Actual, _FunName}, _Context) ->
    explain_arity_mismatch(Expected, Actual);
explain_error({occurs_check, _Var, _Type}, _Context) ->
    explain_infinite_type();
explain_error({unbound_variable, Var}, _Context) ->
    io_lib:format("The variable '~s' is not defined in this scope.\n"
                  "Check for typos or make sure the variable is bound before use.",
                  [Var]);
explain_error({ambiguous_type, _Var}, _Context) ->
    "The type cannot be determined uniquely from the context.\n"
    "Consider adding a type annotation to disambiguate.";
explain_error(_Other, _Context) ->
    [].

%%%===================================================================
%%% Internal Functions - Type Mismatch Explanations
%%%===================================================================

explain_type_mismatch({tfun, _, _, _}, Type, _Context) when element(1, Type) =/= tfun ->
    "You're trying to use a non-function value where a function is expected.\n"
    "Functions are called with parentheses: function_name(args).";

explain_type_mismatch(Type, {tfun, _, _, _}, _Context) when element(1, Type) =/= tfun ->
    "You're trying to use a function where a regular value is expected.\n"
    "Did you forget to call the function with arguments?";

explain_type_mismatch({tcon, integer}, {tcon, string}, _Context) ->
    "You're using a string where an integer is expected.\n"
    "Strings are text in quotes, integers are numbers without quotes.";

explain_type_mismatch({tcon, string}, {tcon, integer}, _Context) ->
    "You're using an integer where a string is expected.\n"
    "If you meant to use the number as text, wrap it in quotes.";

explain_type_mismatch({tcon, bool}, Type, _Context) ->
    io_lib:format("Expected a boolean (True or False) but got ~s.\n"
                  "Boolean values are used in conditionals and logical operations.",
                  [type_name(Type)]);

explain_type_mismatch({tapp, {tcon, list}, ElemType1},
                     {tapp, {tcon, list}, ElemType2}, _Context) ->
    io_lib:format("List element type mismatch:\n"
                  "  Expected list of ~s\n"
                  "  Got list of ~s\n"
                  "All elements in a list must have the same type.",
                  [type_name(ElemType1), type_name(ElemType2)]);

explain_type_mismatch({tapp, {tcon, list}, _}, {tapp, {tcon, maybe_type}, _}, _Context) ->
    "Lists and Maybe values are different container types:\n"
    "  - List: Contains zero or more elements [a, b, c]\n"
    "  - Maybe: Contains zero or one element (Some value | None)\n"
    "Consider using 'map' to transform container contents.";

explain_type_mismatch({trecord, Fields1, _}, {trecord, Fields2, _}, _Context) ->
    explain_record_mismatch(Fields1, Fields2);

explain_type_mismatch({tvariant, _Ctors1}, {tvariant, _Ctors2}, _Context) ->
    "Variant types are incompatible. Each variant is a distinct type.\n"
    "Check that you're using the correct variant type for this context.";

explain_type_mismatch({ttuple, Elems1}, {ttuple, Elems2}, _Context)
  when length(Elems1) =/= length(Elems2) ->
    io_lib:format("Tuple size mismatch:\n"
                  "  Expected ~B-tuple\n"
                  "  Got ~B-tuple\n"
                  "Tuples must have exactly the right number of elements.",
                  [length(Elems1), length(Elems2)]);

explain_type_mismatch(_T1, _T2, _Context) ->
    "The types are incompatible and cannot be unified.\n"
    "Check the expected type signature and ensure your value matches.".

explain_record_mismatch(Fields1, Fields2) ->
    F1Names = [Name || {Name, _} <- Fields1],
    F2Names = [Name || {Name, _} <- Fields2],
    Missing = F1Names -- F2Names,
    Extra = F2Names -- F1Names,

    case {Missing, Extra} of
        {[], []} ->
            "Record field types don't match.\n"
            "Check that each field has the correct type.";
        {[], Extra} ->
            io_lib:format("Record has unexpected fields: ~p\n"
                         "Remove these fields or use a different record type.",
                         [Extra]);
        {Missing, []} ->
            io_lib:format("Record is missing required fields: ~p\n"
                         "Add the missing fields with appropriate values.",
                         [Missing]);
        {Missing, Extra} ->
            io_lib:format("Record structure mismatch:\n"
                         "  Missing fields: ~p\n"
                         "  Unexpected fields: ~p",
                         [Missing, Extra])
    end.

%%%===================================================================
%%% Internal Functions - Other Error Explanations
%%%===================================================================

explain_missing_trait(Trait) ->
    CommonTraits = #{
        'Eq' => "equality comparison (==)",
        'Ord' => "ordering comparison (<, >, <=, >=)",
        'Show' => "conversion to string for display",
        'Functor' => "mapping over container contents",
        'Monad' => "sequencing effectful computations"
    },

    case maps:find(Trait, CommonTraits) of
        {ok, Purpose} ->
            io_lib:format("The type doesn't support ~s (~s).\n"
                         "This trait enables ~s operations.",
                         [Trait, Purpose, Purpose]);
        error ->
            io_lib:format("The type doesn't have an instance of trait '~s'.\n"
                         "This trait is required for the operation.",
                         [Trait])
    end.

explain_arity_mismatch(Expected, Actual) when Actual > Expected ->
    io_lib:format("Too many arguments:\n"
                  "  Expected: ~B argument~s\n"
                  "  Provided: ~B argument~s\n"
                  "Remove the extra argument~s.",
                  [Expected, plural(Expected),
                   Actual, plural(Actual),
                   plural(Actual - Expected)]);

explain_arity_mismatch(Expected, Actual) when Actual < Expected ->
    io_lib:format("Not enough arguments:\n"
                  "  Expected: ~B argument~s\n"
                  "  Provided: ~B argument~s\n"
                  "Add the missing argument~s.",
                  [Expected, plural(Expected),
                   Actual, plural(Actual),
                   plural(Expected - Actual)]).

explain_infinite_type() ->
    "Cannot create an infinite (recursive) type.\n"
    "This happens when a type refers to itself without a base case.\n"
    "\n"
    "Common causes:\n"
    "  - Missing base case in recursive type definition\n"
    "  - Accidental self-reference in type alias\n"
    "  - Incorrect type variable usage".

%%%===================================================================
%%% Internal Functions - Fix Suggestions
%%%===================================================================

suggest_type_mismatch_fix({tcon, integer}, {tcon, string}, _Context) ->
    "To fix: Remove quotes from the number, or use Text.to_integer to convert";

suggest_type_mismatch_fix({tcon, string}, {tcon, integer}, _Context) ->
    "To fix: Add quotes around the number, or use Integer.to_text to convert";

suggest_type_mismatch_fix({tfun, _, _, _}, _NonFun, _Context) ->
    "To fix: Pass a function as the argument, or remove the function call";

suggest_type_mismatch_fix(_T1, _T2, _Context) ->
    "To fix: Check the type signature and adjust your code to match".

suggest_missing_instance_fix(Trait, [Type]) ->
    TypeStr = catena_type_pp:pp_type(Type),
    io_lib:format("To fix this, add an instance declaration:\n\n"
                  "instance ~s ~s where\n"
                  "  -- implement required methods here\n\n"
                  "Or derive it automatically (if supported):\n\n"
                  "type ~s = ... derives [~s]",
                  [Trait, TypeStr, TypeStr, Trait]);

suggest_missing_instance_fix(Trait, Types) ->
    TypesStr = string:join([catena_type_pp:pp_type(T) || T <- Types], ", "),
    io_lib:format("To fix: Implement '~s' for type(s): ~s",
                  [Trait, TypesStr]).

suggest_arity_fix(Expected, Actual) when Actual > Expected ->
    io_lib:format("To fix: Remove ~B argument~s from the function call",
                  [Actual - Expected, plural(Actual - Expected)]);

suggest_arity_fix(Expected, Actual) when Actual < Expected ->
    io_lib:format("To fix: Add ~B more argument~s to the function call",
                  [Expected - Actual, plural(Expected - Actual)]).

suggest_infinite_type_fix() ->
    "To fix:\n"
    "  1. Add a wrapper type to break the cycle (e.g., type Wrapper = W(Type))\n"
    "  2. Use an explicit recursive type with a base case\n"
    "  3. Check for typos in type variable names".

suggest_unbound_variable_fix(Var) ->
    io_lib:format("To fix:\n"
                  "  1. Check spelling of '~s'\n"
                  "  2. Ensure the variable is defined before use\n"
                  "  3. Import the module that exports '~s'",
                  [Var, Var]).

suggest_ambiguous_type_fix() ->
    "To fix: Add an explicit type annotation:\n"
    "  let x : Integer = ...\n"
    "  or\n"
    "  (expression : ExpectedType)".

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Get a human-readable name for a type
type_name({tcon, Name}) -> atom_to_list(Name);
type_name({tvar, Var}) -> io_lib:format("type variable ~s", [Var]);
type_name({tapp, {tcon, Container}, _}) ->
    io_lib:format("~s", [Container]);
type_name({tfun, _, _, _}) -> "function";
type_name({trecord, _, _}) -> "record";
type_name({tvariant, _}) -> "variant";
type_name({ttuple, Elems}) ->
    io_lib:format("~B-tuple", [length(Elems)]);
type_name(_) -> "value".

%% @doc Pluralization helper
plural(1) -> "";
plural(_) -> "s".