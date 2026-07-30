-module(catena_control_validate_tests).

-include_lib("eunit/include/eunit.hrl").

valid_graph_produces_retained_report_test() ->
    {ok, Unit} = explicit_unit(),
    Report = catena_compilation_unit:control_validation(Unit),
    ?assert(catena_control_validate:is_report(Report)),
    ?assertEqual(passed, maps:get(status, Report)),
    ?assert(maps:get(nodes, Report) > 0),
    ?assertEqual(1, maps:get(delimiters, Report)).

wrong_continuation_arity_is_rejected_test() ->
    {ok, Unit} = explicit_unit(),
    IR = mutate_first_node(
        catena_compilation_unit:control_ir(Unit),
        resume,
        fun(Node) ->
            Metadata = maps:get(metadata, Node),
            Node#{metadata => Metadata#{continuation_arity => 0}}
        end
    ),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {continuation_arity_mismatch, resume, 1, 0},
            location := {location, _, _}
        }}},
        validate(IR, Unit)
    ).

dangling_delimiter_and_abort_target_are_rejected_test() ->
    {ok, Unit} = explicit_unit(),
    IR = mutate_first_node(
        catena_compilation_unit:control_ir(Unit),
        resume,
        fun(Node) ->
            Fields = maps:get(fields, Node),
            Node#{fields => Fields#{
                delimiter => {delimiter, missing, 99}
            }}
        end
    ),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {dangling_delimiter, _}
        }}},
        validate(IR, Unit)
    ),
    {ok, AbortUnit} = abort_unit(),
    AbortIR = mutate_first_node(
        catena_compilation_unit:control_ir(AbortUnit),
        abort,
        fun(Node) ->
            Fields = maps:get(fields, Node),
            Node#{fields => Fields#{delimiter => none}}
        end
    ),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {invalid_abort_target, none}
        }}},
        validate(AbortIR, AbortUnit)
    ).

authority_and_kind_mismatches_are_rejected_test() ->
    {ok, Unit} = explicit_unit(),
    NoAuthority = mutate_first_node(
        catena_compilation_unit:control_ir(Unit),
        resume,
        fun(Node) ->
            Fields = maps:get(fields, Node),
            Node#{fields => Fields#{authority => #{}}}
        end
    ),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {resume_without_authority, #{}}
        }}},
        validate(NoAuthority, Unit)
    ),
    WrongKind = mutate_first_node(
        catena_compilation_unit:control_ir(Unit),
        make_resumption,
        fun(Node) ->
            Fields = maps:get(fields, Node),
            Node#{fields => Fields#{kind => multi_shot}}
        end
    ),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {resumption_kind_mismatch, one_shot, multi_shot}
        }}},
        validate(WrongKind, Unit)
    ).

duplicate_continuation_identities_are_rejected_test() ->
    {ok, Unit} = catena_compile:compile_string_to_unit(
        "module DuplicateContinuations\n"
        "transform run x = let a = x in let b = a in b\n"
    ),
    IR0 = catena_compilation_unit:control_ir(Unit),
    BindContinuations = [
        maps:get(continuation, maps:get(fields, Node))
        || Node <- catena_control_ir:nodes(IR0),
           maps:get(op, Node) =:= bind
    ],
    [First, Second] = BindContinuations,
    IR = replace_term(IR0, Second, First),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {duplicate_continuation, First}
        }}},
        validate(IR, Unit)
    ).

unresolved_bridge_is_rejected_as_abi_mismatch_test() ->
    {ok, Unit} = catena_compile:compile_string_to_unit(
        "module BridgeNegative\n"
        "transform callee x = x\n"
        "transform caller x = callee x\n"
    ),
    IR = mutate_first_node(
        catena_compilation_unit:control_ir(Unit),
        direct_call,
        fun(Node) ->
            Metadata = maps:get(metadata, Node),
            Fields = maps:get(fields, Node),
            Closure = maps:get(closure, Fields),
            Node#{
                op => bridge,
                metadata => Metadata#{
                    runtime_disposition => unresolved_mode_bridge
                },
                fields => Fields#{
                    bridge => resumable_to_direct,
                    proof => missing,
                    closure => Closure#{control_mode => resumable}
                }
            }
        end
    ),
    ?assertMatch(
        {error, {backend_error, resumption_abi_mismatch, #{
            reason := {resumption_abi_mismatch, _}
        }}},
        validate(IR, Unit)
    ).

leaked_control_ast_and_missing_origin_are_rejected_test() ->
    {ok, Unit} = catena_compile:compile_string_to_unit(
        "module LeakageNegative\n"
        "transform run x = x\n"
    ),
    Leaked = mutate_first_node(
        catena_compilation_unit:control_ir(Unit),
        direct_expr,
        fun(Node) ->
            Fields = maps:get(fields, Node),
            Node#{fields => Fields#{
                source => {resume_expr, bad, value, loc()}
            }}
        end
    ),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {leaked_control_ast, direct_expr}
        }}},
        validate(Leaked, Unit)
    ),
    MissingOrigin = mutate_first_node(
        catena_compilation_unit:control_ir(Unit),
        direct_expr,
        fun(Node) ->
            Metadata = maps:get(metadata, Node),
            Node#{metadata => Metadata#{origin => undefined}}
        end
    ),
    ?assertMatch(
        {error, {backend_error, invalid_control_ir, #{
            reason := {missing_control_origin, direct_expr}
        }}},
        validate(MissingOrigin, Unit)
    ).

validate(IR, Unit) ->
    catena_control_validate:validate_ir(
        IR,
        catena_compilation_unit:control_modes(Unit),
        #{
            module => catena_compilation_unit:module_name(Unit),
            source_identity =>
                catena_compilation_unit:source_identity(Unit)
        }
    ).

explicit_unit() ->
    catena_compile:compile_string_to_unit(
        "module ValidateExplicit\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform run = handle perform Choice.choose() then {\n"
        "  Choice { choose() with k -> resume(k, 1) }\n"
        "}\n"
    ).

abort_unit() ->
    catena_compile:compile_string_to_unit(
        "module ValidateAbort\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform run = handle perform Choice.choose() then {\n"
        "  Choice { choose() with k -> 0 }\n"
        "}\n"
    ).

mutate_first_node(IR, Operation, Fun) ->
    {Transforms, true} = rewrite_terms(
        maps:get(transforms, IR),
        Operation,
        Fun,
        false
    ),
    IR#{transforms => Transforms}.

rewrite_terms([], _Operation, _Fun, Changed) ->
    {[], Changed};
rewrite_terms([Term | Rest], Operation, Fun, Changed0) ->
    {NewTerm, Changed1} = rewrite_term(
        Term,
        Operation,
        Fun,
        Changed0
    ),
    {NewRest, Changed2} = rewrite_terms(
        Rest,
        Operation,
        Fun,
        Changed1
    ),
    {[NewTerm | NewRest], Changed2}.

rewrite_term(Term, Operation, Fun, false) when is_map(Term) ->
    case catena_control_ir:is_node(Term) andalso
        maps:get(op, Term) =:= Operation
    of
        true ->
            {Fun(Term), true};
        false ->
            Pairs = maps:to_list(Term),
            {NewPairs, Changed} = rewrite_pairs(
                Pairs,
                Operation,
                Fun,
                false
            ),
            {maps:from_list(NewPairs), Changed}
    end;
rewrite_term(Term, Operation, Fun, false) when is_list(Term) ->
    rewrite_terms(Term, Operation, Fun, false);
rewrite_term(Term, Operation, Fun, false) when is_tuple(Term) ->
    {Elements, Changed} = rewrite_terms(
        tuple_to_list(Term),
        Operation,
        Fun,
        false
    ),
    {list_to_tuple(Elements), Changed};
rewrite_term(Term, _Operation, _Fun, Changed) ->
    {Term, Changed}.

replace_term(Term, Old, New) when Term =:= Old ->
    New;
replace_term(Term, Old, New) when is_map(Term) ->
    maps:from_list([
        {Key, replace_term(Value, Old, New)}
        || {Key, Value} <- maps:to_list(Term)
    ]);
replace_term(Term, Old, New) when is_list(Term) ->
    [replace_term(Value, Old, New) || Value <- Term];
replace_term(Term, Old, New) when is_tuple(Term) ->
    list_to_tuple([
        replace_term(Value, Old, New)
        || Value <- tuple_to_list(Term)
    ]);
replace_term(Term, _Old, _New) ->
    Term.

rewrite_pairs([], _Operation, _Fun, Changed) ->
    {[], Changed};
rewrite_pairs([{Key, Value} | Rest], Operation, Fun, Changed0) ->
    {NewValue, Changed1} = rewrite_term(
        Value,
        Operation,
        Fun,
        Changed0
    ),
    {NewRest, Changed2} = rewrite_pairs(
        Rest,
        Operation,
        Fun,
        Changed1
    ),
    {[{Key, NewValue} | NewRest], Changed2}.

loc() ->
    {location, 1, 1}.
