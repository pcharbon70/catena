%%%-------------------------------------------------------------------
%%% @doc Phase 14.4 validation and verification for algebraic effects.
%%%
%%% This module turns the existing runtime, type-system, higher-order,
%%% and property-testing infrastructure into a single validation surface.
%%% It produces structured reports for:
%%%
%%% - theoretical validation
%%% - property-based validation
%%% - conformance validation
%%%
%%% The checks are intentionally deterministic so they can be used in
%%% CI and in section-level phase sign-off.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_validation).

-export([
    validate/0,
    validate/1,
    theoretical_validation/0,
    property_validation/0,
    conformance_validation/0
]).

-export([
    validate_law_sets/1,
    validate_handler_models/0,
    validate_row_polymorphism/0,
    validate_higher_order_effects/0,
    validate_runtime_conformance/0,
    validate_operation_conformance/0,
    validate_effect_type_conformance/0,
    validate_codegen_conformance/0
]).

-type validation_section() :: theoretical | properties | conformance | all.
-type validation_check() :: #{
    name := atom(),
    passed := boolean(),
    details := term()
}.
-type validation_report() :: #{
    section := validation_section(),
    passed := boolean(),
    checks := [validation_check()]
}.
-type aggregate_report() :: #{
    section := all,
    passed := boolean(),
    reports := [validation_report()],
    checks := [validation_check()]
}.

-export_type([
    validation_section/0,
    validation_check/0,
    validation_report/0,
    aggregate_report/0
]).

%%====================================================================
%% Public API
%%====================================================================

-spec validate() -> aggregate_report().
validate() ->
    validate(all).

-spec validate(validation_section()) -> validation_report() | aggregate_report().
validate(theoretical) ->
    theoretical_validation();
validate(properties) ->
    property_validation();
validate(conformance) ->
    conformance_validation();
validate(all) ->
    Reports = [
        theoretical_validation(),
        property_validation(),
        conformance_validation()
    ],
    AllChecks = lists:append([maps:get(checks, Report) || Report <- Reports]),
    #{
        section => all,
        passed => lists:all(fun report_passed/1, Reports),
        reports => Reports,
        checks => AllChecks
    }.

-spec theoretical_validation() -> validation_report().
theoretical_validation() ->
    build_report(theoretical, [
        run_check(law_sets_well_formed, fun() ->
            validate_law_sets(catena_algebraic_laws:list_law_sets())
        end),
        run_check(handler_models_sound, fun validate_handler_models/0),
        run_check(row_polymorphism_sound, fun validate_row_polymorphism/0),
        run_check(higher_order_effects_sound, fun validate_higher_order_effects/0)
    ]).

-spec property_validation() -> validation_report().
property_validation() ->
    build_report(properties, [
        property_check(
            row_union_commutative,
            row_union_commutative_property(),
            catena_gen:seed_from_int(140401),
            30
        ),
        property_check(
            handler_echo_roundtrip,
            handler_echo_roundtrip_property(),
            catena_gen:seed_from_int(140402),
            30
        ),
        property_check(
            row_constraints_preserve_required_effects,
            row_constraint_satisfaction_property(),
            catena_gen:seed_from_int(140403),
            30
        )
    ]).

-spec conformance_validation() -> validation_report().
conformance_validation() ->
    build_report(conformance, [
        run_check(runtime_contract, fun validate_runtime_conformance/0),
        run_check(operation_contract, fun validate_operation_conformance/0),
        run_check(effect_type_contract, fun validate_effect_type_conformance/0),
        run_check(codegen_contract, fun validate_codegen_conformance/0)
    ]).

%%====================================================================
%% Theoretical Validation Checks
%%====================================================================

-spec validate_law_sets([atom()]) -> {boolean(), term()}.
validate_law_sets(SetNames) ->
    Results = [{Name, validate_law_set(Name)} || Name <- SetNames],
    {lists:all(fun({_Name, Result}) -> Result =:= ok end, Results), Results}.

validate_law_set(Name) ->
    Set = catena_algebraic_laws:get_law_set(Name),
    catena_equation_spec:validate_set(Set).

-spec validate_handler_models() -> {boolean(), map()}.
validate_handler_models() ->
    DeepHandler = #{
        effect => state,
        handler => fun(_Op, _Args) -> deep end,
        scope => make_ref(),
        depth => 0
    },
    ShallowHandler = #{
        effect => state,
        handler => fun(_Op, _Args) -> shallow end,
        scope => make_ref()
    },
    DeepContext = #{
        handlers => [DeepHandler],
        stack => [{state, DeepHandler}],
        cache => #{}
    },
    ShallowContext = #{
        handlers => [ShallowHandler],
        depth => 1
    },
    DeepResult = catena_deep_handler:execute_deep({state, get, []}, DeepContext, 3),
    ShallowDirect = catena_shallow_handler:execute_shallow({state, get, []}, ShallowContext, 1),
    ShallowNested = catena_shallow_handler:execute_shallow({state, get, []}, ShallowContext, 2),
    Passed =
        DeepResult =:= {handled, deep} andalso
        ShallowDirect =:= {handled, shallow} andalso
        ShallowNested =:= {unhandled, {state, get, []}},
    {Passed, #{
        deep => DeepResult,
        shallow_direct => ShallowDirect,
        shallow_nested => ShallowNested
    }}.

-spec validate_row_polymorphism() -> {boolean(), map()}.
validate_row_polymorphism() ->
    Row1 = catena_row_types:effect_row(['IO', 'State']),
    Row2 = catena_row_types:effect_row(['State', 'Error']),
    Union = catena_row_types:row_union(Row1, Row2),
    Difference = catena_row_types:row_difference(Union, Row2),
    Constraint = catena_row_poly_integration:row_constraint({row_var, 1}, ['IO']),
    ConstraintSatisfied =
        catena_row_poly_integration:satisfy_row_constraints([Constraint], Union),
    Passed =
        catena_row_types:row_contains_all(Union, Row1) andalso
        catena_row_types:row_contains_all(Union, Row2) andalso
        ConstraintSatisfied andalso
        lists:sort(catena_row_types:row_to_list(Difference)) =:= ['IO'],
    {Passed, #{
        row1 => catena_row_types:row_to_list(Row1),
        row2 => catena_row_types:row_to_list(Row2),
        union => catena_row_types:row_to_list(Union),
        difference => catena_row_types:row_to_list(Difference),
        constraint_satisfied => ConstraintSatisfied
    }}.

-spec validate_higher_order_effects() -> {boolean(), map()}.
validate_higher_order_effects() ->
    RowVar = catena_op_signatures:fresh_row_var(rho),
    Effects = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => RowVar
    },
    Sig = catena_op_signatures:op_sig(iterate, [atom], integer, Effects),
    Scope = catena_op_signatures:scope_add_var(
        catena_op_signatures:new_scope(),
        RowVar
    ),
    EscapeCheck = catena_op_signatures:check_row_var_escape(Sig, Scope),
    Required = catena_op_signatures:required_effects(Sig),
    ConstraintOk = catena_op_signatures:satisfies_constraints(['IO', 'State'], Sig),
    HOParam = catena_ho_effects:effectful_param(atom, integer),
    HOType = catena_ho_effects:ho_op_type(iterate, [HOParam], integer, Effects),
    GeneralizedSig = catena_op_signatures:generalize_sig(
        catena_op_signatures:op_sig(simple, [], integer, #{
            kind => effect_row,
            elements => [],
            row_var => undefined
        })
    ),
    GeneralizedEffects = catena_op_signatures:sig_effects(GeneralizedSig),
    Passed =
        matches_ok(EscapeCheck) andalso
        Required =:= ['IO'] andalso
        ConstraintOk andalso
        catena_ho_effects:is_ho_op(HOType) andalso
        catena_ho_effects:count_effectful_params(HOType) =:= 1 andalso
        maps:get(row_var, GeneralizedEffects, undefined) =/= undefined,
    {Passed, #{
        escape_check => EscapeCheck,
        required_effects => Required,
        constraints_satisfied => ConstraintOk,
        higher_order_params => catena_ho_effects:count_effectful_params(HOType),
        generalized_row_var => maps:get(row_var, GeneralizedEffects, undefined)
    }}.

%%====================================================================
%% Conformance Validation Checks
%%====================================================================

-spec validate_runtime_conformance() -> {boolean(), map()}.
validate_runtime_conformance() ->
    catch catena_effect_system:shutdown(),
    Before = catena_effect_system:is_initialized(),
    Result = catena_effect_system:with_runtime(
        [{optimization_level, 3}],
        fun(Ctx) ->
            catena_effect_system:is_initialized() andalso
            maps:get(handlers, Ctx) =:= #{} andalso
            maps:get(parent, Ctx) =:= undefined
        end
    ),
    After = catena_effect_system:is_initialized(),
    Passed = (Before =:= false) andalso Result andalso (After =:= false),
    {Passed, #{
        before => Before,
        body_result => Result,
        'after' => After
    }}.

-spec validate_operation_conformance() -> {boolean(), map()}.
validate_operation_conformance() ->
    Ctx = catena_effect_runtime:empty_context(),
    Builtin = catena_effect_runtime:perform(Ctx, 'Process', self, []),
    Handled = catena_effect_runtime:with_handlers(
        Ctx,
        [{'Math', [{add, fun(A, B) -> A + B end}]}],
        fun(ChildCtx) ->
            catena_effect_runtime:perform(ChildCtx, 'Math', add, [2, 3])
        end
    ),
    Passed = (Builtin =:= self()) andalso (Handled =:= 5),
    {Passed, #{
        builtin => Builtin,
        handled => Handled
    }}.

-spec validate_effect_type_conformance() -> {boolean(), map()}.
validate_effect_type_conformance() ->
    PerformExpr = {perform_expr, 'IO', read, [], loc()},
    HandleExpr = {handle_expr,
        PerformExpr,
        [{handler_clause, 'IO', [
            {operation_case, read, [], {literal, integer, 0, loc()}, loc()}
        ], loc()}],
        loc()},
    {PerformEffects, _} =
        catena_effect_synthesis:synthesize(PerformExpr, catena_infer_state:new()),
    {HandleEffects, _} =
        catena_effect_synthesis:synthesize(HandleExpr, catena_infer_state:new()),
    SubsetResult = catena_effect_constraints:solve_effect_constraint(
        {effects_subset, declared_effects,
            catena_types:effect_set(['IO']),
            catena_types:effect_set(['IO', 'State'])},
        catena_types:empty_effects(),
        catena_infer_state:new()
    ),
    Passed =
        catena_effect_synthesis:contains(PerformEffects, 'IO') andalso
        not catena_effect_synthesis:contains(HandleEffects, 'IO') andalso
        matches_ok(SubsetResult),
    {Passed, #{
        perform_effects => catena_effect_synthesis:to_list(PerformEffects),
        handled_effects => catena_effect_synthesis:to_list(HandleEffects),
        subset_result => SubsetResult
    }}.

-spec validate_codegen_conformance() -> {boolean(), map()}.
validate_codegen_conformance() ->
    Expr = {perform_expr, 'IO', println, [
        {literal, string, <<"hello">>, loc()}
    ], loc()},
    {PerformCore, _} = catena_effect_codegen:translate_perform(
        Expr,
        catena_codegen_utils:new_state()
    ),
    Decl = {transform, log_value, [{pat_var, value, loc()}],
        {perform_expr, 'IO', println, [{var, value, loc()}], loc()},
        loc()},
    {{_FName, FunDef}, _} = catena_codegen_module:compile_function(
        Decl,
        catena_codegen_utils:new_state()
    ),
    WrappedBody = cerl:fun_body(FunDef),
    Passed =
        cerl:atom_val(cerl:call_module(PerformCore)) =:= catena_effect_runtime andalso
        cerl:atom_val(cerl:call_name(PerformCore)) =:= perform andalso
        cerl:atom_val(cerl:call_module(WrappedBody)) =:= catena_effect_system andalso
        cerl:atom_val(cerl:call_name(WrappedBody)) =:= with_runtime,
    {Passed, #{
        perform_target => {
            cerl:atom_val(cerl:call_module(PerformCore)),
            cerl:atom_val(cerl:call_name(PerformCore))
        },
        wrapper_target => {
            cerl:atom_val(cerl:call_module(WrappedBody)),
            cerl:atom_val(cerl:call_name(WrappedBody))
        }
    }}.

%%====================================================================
%% Property Checks
%%====================================================================

property_check(Name, Property, Seed, NumTests) ->
    case catena_runner:run_property(Property, #{num_tests => NumTests, seed => Seed}) of
        {passed, Result} ->
            #{
                name => Name,
                passed => true,
                details => #{status => passed, result => Result}
            };
        {failed, Result} ->
            #{
                name => Name,
                passed => false,
                details => #{status => failed, result => Result}
            }
    end.

row_union_commutative_property() ->
    catena_property:property(<<"row_union_commutative">>, fun() ->
        catena_property:forall(
            catena_stdgen:gen_pair(effect_list_gen(), effect_list_gen()),
            fun(EffectsA, EffectsB) ->
                RowA = catena_row_types:effect_row(EffectsA),
                RowB = catena_row_types:effect_row(EffectsB),
                normalize_effects(catena_row_types:row_to_list(catena_row_types:row_union(RowA, RowB))) =:=
                    normalize_effects(catena_row_types:row_to_list(catena_row_types:row_union(RowB, RowA)))
            end
        )
    end).

handler_echo_roundtrip_property() ->
    catena_property:property(<<"handler_echo_roundtrip">>, fun() ->
        catena_property:forall(
            catena_gen:gen_int(),
            fun(Value) ->
                Ctx = catena_effect_runtime:empty_context(),
                catena_effect_runtime:with_handlers(
                    Ctx,
                    [{'Echo', [{id, fun(X) -> X end}]}],
                    fun(ChildCtx) ->
                        catena_effect_runtime:perform(ChildCtx, 'Echo', id, [Value])
                    end
                ) =:= Value
            end
        )
    end).

row_constraint_satisfaction_property() ->
    catena_property:property(<<"row_constraint_satisfaction">>, fun() ->
        catena_property:forall(
            catena_stdgen:gen_pair(effect_list_gen(), effect_list_gen()),
            fun(RequiredEffects, ExtraEffects) ->
                Required = normalize_effects(RequiredEffects),
                RequiredRow = catena_row_types:effect_row(Required),
                ExtraRow = catena_row_types:effect_row(ExtraEffects),
                Combined = catena_row_types:row_union(RequiredRow, ExtraRow),
                Constraint = catena_row_poly_integration:row_constraint(
                    {row_var, 1},
                    Required
                ),
                catena_row_poly_integration:satisfy_row_constraints([Constraint], Combined)
            end
        )
    end).

%%====================================================================
%% Internal Helpers
%%====================================================================

build_report(Section, Checks) ->
    #{
        section => Section,
        passed => lists:all(fun check_passed/1, Checks),
        checks => Checks
    }.

run_check(Name, Fun) ->
    try
        {Passed, Details} = Fun(),
        #{
            name => Name,
            passed => Passed,
            details => Details
        }
    catch
        Class:Reason:Stack ->
            #{
                name => Name,
                passed => false,
                details => #{
                    error => {Class, Reason},
                    stack => Stack
                }
            }
    end.

report_passed(Report) ->
    maps:get(passed, Report).

check_passed(Check) ->
    maps:get(passed, Check).

matches_ok({ok, _}) ->
    true;
matches_ok(ok) ->
    true;
matches_ok(_) ->
    false.

effect_list_gen() ->
    catena_stdgen:gen_list(
        catena_gen:elements(['IO', 'State', 'Error', 'Async'])
    ).

normalize_effects(Effects) ->
    lists:sort(lists:usort(Effects)).

loc() ->
    {location, 1, 1}.
